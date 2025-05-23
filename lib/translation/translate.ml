open Heaplang
open Llvmir
open Hp_ast
open Llvm_ast

let var_type = Hashtbl.create 10

let param_type = Hashtbl.create 10

let fun_type = Hashtbl.create 10

let type_tbl = Hashtbl.create 10

let tmp_var ?(step = 0) blks blk =
  let total_instructions =
    List.fold_left (fun acc blk -> acc + List.length blk.instructions) 0 blks
  in
  let index = List.length blk.instructions in
  let var_name =
    Printf.sprintf "tmp_%d_%d" (index + total_instructions) step
  in
  var_name

(* This module translates Heaplang expressions into LLVM IR. *)

let fst_type = function
  | Pair (t1, _) -> t1
  | t ->
      Format.printf "%s" (typ_to_string t) ;
      failwith "Expected a pair type"

let snd_type = function
  | Pair (_, t2) -> t2
  | t ->
      Format.printf "%s" (typ_to_string t) ;
      failwith "Expected a pair type"

(* Counter generate *)

let return_type ty =
  match ty with
  | Function (args, ret_type) -> ret_type
  | _ -> failwith "Expected a function type"

let rec translateType ht =
  match ht with
  | TInt -> Int 32
  | TBool -> Int 1
  | TUnit -> Void
  | TLoc ht' -> Pointer (translateType ht')
  | TFun tys ->
      let param_types = List.map translateType (List.tl (List.rev tys)) in
      let ret_type = translateType (List.hd (List.rev tys)) in
      Function (List.rev param_types, ret_type)
  | TPair (t1, t2) -> Pair (translateType t1, translateType t2)
  | TVar name -> LVar name
  | _ -> failwith "Translation not implemented for this type"

let rec decide_ty e =
  match e with
  | Val (LitV (LitInt _)) -> Int 32
  | Val (LitV (LitBool _)) -> Int 1
  | Val (LitV LitUnit) -> Void
  | Val (LitV (LitLoc (0, typ))) -> translateType typ
  | Var v -> (
    match Hashtbl.find_opt var_type v with
    | Some typ -> typ
    | None -> (
      match Hashtbl.find_opt param_type v with
      | Some typ -> typ
      | None -> (
        match Hashtbl.find_opt fun_type v with
        | Some typ -> typ
        | None ->
            Format.printf "Variable not found: %s\n" v ;
            failwith "Fail to find variable type" ) ) )
  (* TODO: consider sz with the type. Currently, only sz = 1 is supported. *)
  | AllocN (_sz, expr) ->
      let typ = decide_ty expr in
      Pointer typ
  | Load ptr -> (
      let typ = decide_ty ptr in
      match typ with
      | Pointer ptyp -> ptyp
      | _ -> failwith "Expected a pointer type for Load" )
  | BinOp (PlusOp, e1, _e2) -> decide_ty e1
  | BinOp (QuotOp, e1, _e2) -> decide_ty e1
  | BinOp (RemOp, e1, _e2) -> decide_ty e1
  | Pair (v1, v2) -> (
      let typ1 = decide_ty v1 in
      let typ2 = decide_ty v2 in
      let pty = Pair (typ1, typ2) in
      let name =
        Hashtbl.fold
          (fun n t acc -> if t = pty then Some n else acc)
          type_tbl None
      in
      match name with None -> pty | Some name -> LVar name )
  | Fst v ->
      let typ = decide_ty v in
      let typ' =
        match typ with
        | LVar name -> (
          match Hashtbl.find_opt type_tbl name with
          | None -> typ
          | Some t -> t )
        | _ -> typ
      in
      let typ1 = fst_type typ' in
      typ1
  | Snd v ->
      let typ = decide_ty v in
      let typ' =
        match typ with
        | LVar name -> (
          match Hashtbl.find_opt type_tbl name with
          | None -> typ
          | Some t -> t )
        | _ -> typ
      in
      let typ2 = snd_type typ' in
      typ2
  | App (Var f, _) -> return_type (decide_ty (Var f))
  | App (f, _) -> decide_ty f
  | _ -> failwith "Type not supported in translation"

let var_name = function Var v -> v | _ -> failwith "Expected a variable"

let extend_blk blk instrs = {blk with instructions= blk.instructions @ instrs}

let rec to_pair_ty typs =
  match typs with
  | [] -> failwith "typs cannot be empty"
  | [ty] -> translateType ty
  | ty1 :: res_t -> Pair (translateType ty1, to_pair_ty res_t)

let rec translateHeaplang stmts =
  match stmts with
  | [] -> {globals= []; user_typs= []; functions= []}
  | st :: rest_stmts -> (
    match st with
    | Rec _ ->
        let fst_st = translateGlobal st in
        let m = translateHeaplang rest_stmts in
        {m with functions= fst_st :: m.functions}
    | TypeDef (name, typs) ->
        Hashtbl.add type_tbl name (to_pair_ty typs) ;
        let m = translateHeaplang rest_stmts in
        {m with user_typs= {name; typ= to_pair_ty typs} :: m.user_typs}
    | _ -> failwith "unsupported top-level constructs" )

and translateApp app varName curr_blk blks =
  match app with
  | App (f, arg) -> (
      let typ = decide_ty arg in
      let curr_blk1, arg_exp = translateExpr arg curr_blk blks in
      let curr_blk2, call_expr = translateApp f varName curr_blk1 blks in
      match call_expr with
      | Call (varName, fun_name, "", args, ret_typ) ->
          ( curr_blk2
          , Call (varName, fun_name, "", args @ [(arg_exp, typ)], ret_typ) )
      | _ -> failwith "Expected a function call" )
  | Var fun_name ->
      let curr_blk1, fun_id = translateExpr (Var fun_name) curr_blk blks in
      let ret_typ = return_type (decide_ty (Var fun_name)) in
      (curr_blk1, Call (varName, fun_id, "", [], ret_typ))
  | _ -> failwith "Expected an application"

and translateGlobal exp =
  let rec aux f ff =
    match f with
    | Rec (BAnon, BNamed x, body, TFun typs) ->
        let param_typ = translateType (List.hd typs) in
        let ret_typ = translateType (List.hd (List.rev typs)) in
        Hashtbl.add param_type x param_typ ;
        let f_def =
          aux body
            {ff with ret_type= ret_typ; params= ff.params @ [(x, param_typ)]}
        in
        f_def
    | _ ->
        { ff with
          basic_blocks= translateBlock f [] {label= "entry"; instructions= []}
        }
  in
  match exp with
  | Rec (BNamed f, BNamed x, body, TFun typs) ->
      let param_typ = translateType (List.hd typs) in
      let ret_typ = translateType (List.hd (List.rev typs)) in
      Hashtbl.add fun_type f (translateType (TFun typs)) ;
      if param_typ = Void then
        let f = {name= f; ret_type= ret_typ; params= []; basic_blocks= []} in
        aux body f
      else (
        Hashtbl.add param_type x param_typ ;
        let f =
          { name= f
          ; ret_type= ret_typ
          ; params= [(x, param_typ)]
          ; basic_blocks= [] }
        in
        aux body f )
  | _ -> failwith "Expected a recursive function"

and translateBlock exp blks curr_blk =
  match exp with
  | Let (x, e1, e2) ->
      let varName = var_name x in
      let typ = decide_ty e1 in
      Hashtbl.add var_type varName typ ;
      let curr_blk1, operand = translateExpr e1 curr_blk blks in
      let curr_blk2 =
        extend_blk curr_blk1
          [ Alloca (varName, typ)
          ; Store (operand, typ, LocalVar varName, Pointer typ) ]
      in
      translateBlock e2 blks curr_blk2
  | Print v ->
      let curr_blk1, operand = translateExpr v curr_blk blks in
      let curr_blk2 =
        extend_blk curr_blk1
          [ GetElementPtr
              ( Some (tmp_var blks curr_blk1)
              , GlobalVar ".str"
              , [IndexConst 0; IndexConst 0]
              , Array (4, Int 8) )
          ; Call
              ( Some (tmp_var ~step:1 blks curr_blk1)
              , GlobalVar "printf"
              , "(i8*, ...)"
              , [ (LocalVar (tmp_var blks curr_blk1), Pointer (Int 8))
                ; (operand, decide_ty v) ]
              , Int 32 ) ]
      in
      blks @ [curr_blk2]
  | Store (e1, e2) ->
      let curr_blk1, operand1 = translateExpr e1 curr_blk blks in
      let curr_blk2, operand2 = translateExpr e2 curr_blk1 blks in
      blks
      @ [ extend_blk curr_blk2
            [Store (operand2, decide_ty e2, operand1, decide_ty e1)] ]
  | Var v ->
      let typ = decide_ty (Var v) in
      let curr_blk1, operand = translateExpr (Var v) curr_blk blks in
      blks @ [extend_blk curr_blk1 [Ret (Some (operand, typ))]]
  | Val v ->
      let typ = decide_ty (Val v) in
      blks @ [extend_blk curr_blk [Ret (Some (translateVal v, typ))]]
  | Fst v ->
      let curr_blk1, operand = translateExpr v curr_blk blks in
      let typ = decide_ty v in
      blks
      @ [ extend_blk curr_blk1
            [ ExtractValue (Some (tmp_var blks curr_blk1), operand, 0, typ)
            ; Ret
                (Some (LocalVar (tmp_var blks curr_blk1), decide_ty (Fst v)))
            ] ]
  | Snd v ->
      let curr_blk1, operand = translateExpr v curr_blk blks in
      let typ = decide_ty v in
      blks
      @ [ extend_blk curr_blk1
            [ ExtractValue (Some (tmp_var blks curr_blk1), operand, 1, typ)
            ; Ret (Some (LocalVar (tmp_var blks curr_blk1), snd_type typ)) ]
        ]
  | App (f, arg) ->
      let typ = decide_ty f in
      let curr_blk11, _ =
        translateApp
          (App (f, arg))
          (Some (tmp_var blks curr_blk))
          curr_blk blks
      in
      let curr_blk1, operand =
        translateApp
          (App (f, arg))
          (Some (tmp_var blks curr_blk11))
          curr_blk blks
      in
      blks
      @ [ extend_blk curr_blk1
            [ operand
            ; Ret (Some (LocalVar (tmp_var blks curr_blk11), return_type typ))
            ] ]
  | If (cond, then_branch, else_branch) ->
      let curr_blk1, cond_operand = translateExpr cond curr_blk blks in
      let curr_blk2 =
        extend_blk curr_blk1 [CondBr (cond_operand, "then", "else")]
      in
      let blks' = blks @ [curr_blk2] in
      let blks'' =
        translateBlock then_branch blks' {label= "then"; instructions= []}
      in
      let blks''' =
        translateBlock else_branch blks'' {label= "else"; instructions= []}
      in
      blks'''
  | BinOp (PlusOp, e1, e2) ->
      let curr_blk1, operand1 = translateExpr e1 curr_blk blks in
      let curr_blk2, operand2 = translateExpr e2 curr_blk1 blks in
      let bin_op =
        BinOp (tmp_var blks curr_blk1, Add, operand1, operand2, decide_ty e1)
      in
      blks
      @ [ extend_blk curr_blk2
            [ bin_op
            ; Ret (Some (LocalVar (tmp_var blks curr_blk1), decide_ty e1)) ]
        ]
  | BinOp (QuotOp, e1, e2) ->
      let curr_blk1, operand1 = translateExpr e1 curr_blk blks in
      let curr_blk2, operand2 = translateExpr e2 curr_blk1 blks in
      let bin_op =
        BinOp (tmp_var blks curr_blk1, Div, operand1, operand2, decide_ty e1)
      in
      blks
      @ [ extend_blk curr_blk2
            [ bin_op
            ; Ret (Some (LocalVar (tmp_var blks curr_blk1), decide_ty e1)) ]
        ]
  | BinOp (RemOp, e1, e2) ->
      let curr_blk1, operand1 = translateExpr e1 curr_blk blks in
      let curr_blk2, operand2 = translateExpr e2 curr_blk1 blks in
      let bin_op =
        BinOp (tmp_var blks curr_blk1, Rem, operand1, operand2, decide_ty e1)
      in
      blks
      @ [ extend_blk curr_blk2
            [ bin_op
            ; Ret (Some (LocalVar (tmp_var blks curr_blk1), decide_ty e1)) ]
        ]
  | Seq (e1, e2) ->
      let blks1 = translateBlock e1 blks curr_blk in
      let blks2 = translateBlock e2 blks (List.hd (List.rev blks1)) in
      blks2
  | Load ptr -> (
      let typ = decide_ty ptr in
      match typ with
      | Pointer ptyp ->
          let curr_blk1, operand = translateExpr ptr curr_blk blks in
          let curr_blk2 =
            extend_blk curr_blk1
              [ Load (Some (tmp_var blks curr_blk1), operand, ptyp)
              ; Ret (Some (LocalVar (tmp_var blks curr_blk1), ptyp)) ]
          in
          blks @ [curr_blk2]
      | _ -> failwith "Expected a pointer type for Load" )
  | Pair (v1, v2) ->
      let curr_blk1, operand1 = translateExpr v1 curr_blk blks in
      let curr_blk2, operand2 = translateExpr v2 curr_blk1 blks in
      let pair_op =
        InsertValue
          ( tmp_var blks curr_blk2
          , Undef
          , (operand1, decide_ty v1)
          , 0
          , decide_ty (Pair (v1, v2)) )
      in
      let pair_op2 =
        InsertValue
          ( tmp_var ~step:1 blks curr_blk2
          , LocalVar (tmp_var blks curr_blk2)
          , (operand2, decide_ty v2)
          , 1
          , decide_ty (Pair (v1, v2)) )
      in
      blks
      @ [ extend_blk curr_blk2
            [ pair_op
            ; pair_op2
            ; Ret
                (Some
                   ( LocalVar (tmp_var ~step:1 blks curr_blk2)
                   , decide_ty (Pair (v1, v2)) ) ) ] ]
  | _ ->
      Format.printf "%s" (ast_to_string exp) ;
      failwith "Translation not implemented for this statement"

and translateExpr e curr_blk blks =
  match e with
  | Val v -> (curr_blk, translateVal v)
  | Var v -> (
    match Hashtbl.find_opt var_type v with
    | Some typ ->
        ( extend_blk curr_blk
            [Load (Some (tmp_var blks curr_blk), LocalVar v, typ)]
        , LocalVar (tmp_var blks curr_blk) )
    | None -> (
      match Hashtbl.find_opt param_type v with
      | Some _ -> (curr_blk, LocalVar v)
      | None -> (
        match Hashtbl.find_opt fun_type v with
        | Some _ -> (curr_blk, GlobalVar v)
        | None -> failwith ("Variable " ^ v ^ " not found in type table") ) )
    )
  | BinOp (EqOp, e1, e2) ->
      let curr_blk1, operand1 = translateExpr e1 curr_blk blks in
      let curr_blk2, operand2 = translateExpr e2 curr_blk1 blks in
      let cmp_op =
        Icmp (tmp_var blks curr_blk2, "eq", operand1, operand2, decide_ty e1)
      in
      (extend_blk curr_blk2 [cmp_op], LocalVar (tmp_var blks curr_blk2))
  | BinOp (PlusOp, op1, op2) ->
      let curr_blk1, operand1 = translateExpr op1 curr_blk blks in
      let curr_blk2, operand2 = translateExpr op2 curr_blk1 blks in
      let bin_op =
        BinOp (tmp_var blks curr_blk2, Add, operand1, operand2, decide_ty op1)
      in
      (extend_blk curr_blk2 [bin_op], LocalVar (tmp_var blks curr_blk2))
  | BinOp (QuotOp, op1, op2) ->
      let curr_blk1, operand1 = translateExpr op1 curr_blk blks in
      let curr_blk2, operand2 = translateExpr op2 curr_blk1 blks in
      let bin_op =
        BinOp (tmp_var blks curr_blk2, Div, operand1, operand2, decide_ty op1)
      in
      (extend_blk curr_blk2 [bin_op], LocalVar (tmp_var blks curr_blk2))
  | BinOp (RemOp, op1, op2) ->
      let curr_blk1, operand1 = translateExpr op1 curr_blk blks in
      let curr_blk2, operand2 = translateExpr op2 curr_blk1 blks in
      let bin_op =
        BinOp (tmp_var blks curr_blk2, Rem, operand1, operand2, decide_ty op1)
      in
      (extend_blk curr_blk2 [bin_op], LocalVar (tmp_var blks curr_blk2))
  | Snd e ->
      let curr_blk1, operand = translateExpr e curr_blk blks in
      let typ = decide_ty e in
      ( extend_blk curr_blk1
          [ExtractValue (Some (tmp_var blks curr_blk1), operand, 1, typ)]
      , LocalVar (tmp_var blks curr_blk1) )
  | Fst e ->
      let curr_blk1, operand = translateExpr e curr_blk blks in
      let typ = decide_ty e in
      ( extend_blk curr_blk1
          [ExtractValue (Some (tmp_var blks curr_blk1), operand, 0, typ)]
      , LocalVar (tmp_var blks curr_blk1) )
  | Load ptr -> (
      let typ = decide_ty ptr in
      match typ with
      | Pointer typ ->
          let curr_blk1, operand = translateExpr ptr curr_blk blks in
          ( extend_blk curr_blk1
              [Load (Some (tmp_var blks curr_blk1), operand, typ)]
          , LocalVar (tmp_var blks curr_blk1) )
      | _ -> failwith "Expected a pointer type for Load" )
  | Pair (v1, v2) ->
      let curr_blk1, operand1 = translateExpr v1 curr_blk blks in
      let curr_blk2, operand2 = translateExpr v2 curr_blk1 blks in
      let pair_op =
        InsertValue
          ( tmp_var blks curr_blk2
          , Undef
          , (operand1, decide_ty v1)
          , 0
          , decide_ty (Pair (v1, v2)) )
      in
      let pair_op2 =
        InsertValue
          ( tmp_var ~step:1 blks curr_blk2
          , LocalVar (tmp_var blks curr_blk2)
          , (operand2, decide_ty v2)
          , 1
          , decide_ty (Pair (v1, v2)) )
      in
      ( extend_blk curr_blk2 [pair_op; pair_op2]
      , LocalVar (tmp_var ~step:1 blks curr_blk2) )
  | AllocN (sz, Pair (v1, v2)) as alloc ->
      (* Currently, the translation supports only sz == 1. *)
      let curr_blk1, operand = translateExpr sz curr_blk blks in
      let typ1 = decide_ty (Pair (v1, v2)) in
      let curr_blk2, operand1 = translateExpr v1 curr_blk1 blks in
      let curr_blk3, operand2 = translateExpr v2 curr_blk2 blks in
      let curr_blk4 =
        extend_blk curr_blk3
          [ InsertValue
              ( tmp_var blks curr_blk3
              , Undef
              , (operand1, decide_ty v1)
              , 0
              , typ1 )
          ; InsertValue
              ( tmp_var ~step:1 blks curr_blk3
              , LocalVar (tmp_var blks curr_blk3)
              , (operand2, decide_ty v2)
              , 1
              , typ1 ) ]
      in
      let curr_blk5 =
        extend_blk curr_blk4
          [ Call
              ( Some (tmp_var blks curr_blk4)
              , GlobalVar "malloc"
              , ""
              , [(operand, Int 64)]
              , decide_ty alloc )
          ; GetElementPtr
              ( Some (tmp_var ~step:1 blks curr_blk4)
              , LocalVar (tmp_var blks curr_blk4)
              , [IndexConst 0]
              , typ1 )
          ; Store
              ( LocalVar (tmp_var ~step:1 blks curr_blk3)
              , typ1
              , LocalVar (tmp_var ~step:1 blks curr_blk4)
              , decide_ty alloc ) ]
      in
      (curr_blk5, LocalVar (tmp_var blks curr_blk4))
  | AllocN (sz, value) as alloc ->
      (* Currently, the translation supports only sz == 1. *)
      let curr_blk1, operand = translateExpr sz curr_blk blks in
      let curr_blk2, valu = translateExpr value curr_blk1 blks in
      let typ1 = decide_ty value in
      let curr_blk3 =
        extend_blk curr_blk2
          [ Call
              ( Some (tmp_var blks curr_blk2)
              , GlobalVar "malloc"
              , ""
              , [(operand, Int 64)]
              , decide_ty alloc )
          ; GetElementPtr
              ( Some (tmp_var ~step:1 blks curr_blk2)
              , LocalVar (tmp_var blks curr_blk2)
              , [IndexConst 0]
              , typ1 )
          ; Store
              ( valu
              , typ1
              , LocalVar (tmp_var ~step:1 blks curr_blk2)
              , decide_ty alloc ) ]
      in
      (curr_blk3, LocalVar (tmp_var blks curr_blk2))
  | App (f, arg) ->
      let curr_blk1, _ = translateApp (App (f, arg)) None curr_blk blks in
      let curr_blk2, operand1 =
        translateApp
          (App (f, arg))
          (Some (tmp_var blks curr_blk1))
          curr_blk blks
      in
      let curr_blk3 = extend_blk curr_blk2 [operand1] in
      (curr_blk3, LocalVar (tmp_var blks curr_blk1))
  | _ -> failwith "Translation not implemented for this expression"

(* Translate Heaplang values to LLVM IR operands *)

and translateVal v =
  match v with
  | LitV (LitInt i) -> ConstInt i
  | LitV (LitBool b) -> ConstInt (if b then 1 else 0)
  | LitV LitUnit -> ConstVoid
  | LitV (LitLoc (l, _typ)) -> ConstLoc l
  | LitV LitPoison -> failwith "Poison value not supported"
  | _ -> failwith "Translation not implemented for this value"
