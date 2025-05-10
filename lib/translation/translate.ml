open Heaplang
open Llvmir
open Hp_ast
open Llvm_ast

let var_type = Hashtbl.create 10

(* This module translates Heaplang expressions into LLVM IR. *)

let rec decide_ty e =
  match e with
  | Val (LitV (LitInt _)) -> Int 32
  | Val (LitV (LitBool _)) -> Int 1
  | Val (LitV LitUnit) -> Void
  | Var v -> (
    match Hashtbl.find_opt var_type v with
    | Some typ -> typ
    | None -> failwith ("Variable " ^ v ^ " not found in type table") )
  (* TODO: consider sz with the type. Currently, only sz = 1 is supported. *)
  | AllocN (_sz, expr) ->
      let typ = decide_ty expr in
      Pointer typ
  | Pair (v1, v2) ->
      let typ1 = decide_ty v1 in
      let typ2 = decide_ty v2 in
      Pair (typ1, typ2)
  | _ -> failwith "Type not supported in translation"

let var_name = function Var v -> v | _ -> failwith "Expected a variable"

let extend_blk blk instrs = {blk with instructions= blk.instructions @ instrs}

let rec translateGlobal exp =
  match exp with
  | Rec (BNamed f, BNamed x, body) ->
      let f =
        { name= f
        ; ret_type= Int 32
        ; params= [(x, Int 32)]
        ; basic_blocks=
            translateBlock body [] {label= "entry"; instructions= []} }
      in
      f
  | _ -> failwith "Expected a recursive function"

and translateBlock exp blks curr_blk =
  match exp with
  | Let (x, e1, e2) -> (
      let varName = var_name x in
      let typ = decide_ty e1 in
      Hashtbl.add var_type varName typ ;
      match e1 with
      | Val (LitV (LitInt _)) ->
          let operand = translateExpr e1 in
          let curr_blk1 =
            extend_blk curr_blk
              [ Alloca ("%" ^ varName, typ)
              ; Store (operand, decide_ty e1, LocalVar varName, Pointer typ)
              ]
          in
          translateBlock e2 blks curr_blk1
      | BinOp _ as bop ->
          let operand = translateExpr bop in
          let curr_blk1 =
            extend_blk curr_blk
              [ Alloca (varName, typ)
              ; Store (operand, decide_ty bop, LocalVar varName, typ) ]
          in
          translateBlock e2 blks curr_blk1
      | Pair (v1, v2) ->
          let operand1 = translateExpr v1 in
          let operand2 = translateExpr v2 in
          let curr_blk1 =
            extend_blk curr_blk
              [ Alloca (varName, typ)
              ; InsertValue (varName ^ "_fst", Undef, operand1, 0, typ)
              ; InsertValue (varName, LocalVar varName, operand2, 1, typ) ]
          in
          translateBlock e2 blks curr_blk1
      | Val (PairV (v1, v2)) ->
          let operand1 = translateVal v1 in
          let operand2 = translateVal v2 in
          let curr_blk1 =
            extend_blk curr_blk
              [ Alloca (varName, typ)
              ; InsertValue (varName ^ "_fst", Undef, operand1, 0, typ)
              ; InsertValue (varName, LocalVar varName, operand2, 1, typ) ]
          in
          translateBlock e2 blks curr_blk1
      | AllocN (sz, Pair (v1, v2)) ->
          (* Currently, the translation supports only sz == 1. *)
          let operand = translateExpr sz in
          let typ1 = decide_ty (Pair (v1, v2)) in
          let operand1 = translateExpr v1 in
          let operand2 = translateExpr v2 in
          let curr_blk1 =
            extend_blk curr_blk
              [ Alloca ("%" ^ varName, typ1)
              ; InsertValue ("%" ^ varName ^ "_fst", Undef, operand1, 0, typ1)
              ; InsertValue
                  ( "%" ^ varName ^ "_snd"
                  , LocalVar varName
                  , operand2
                  , 1
                  , typ1 ) ]
          in
          let curr_blk2 =
            extend_blk curr_blk1
              [ Call
                  (Some ("%" ^ varName), "malloc", [(operand, Int 64)], typ)
              ; GetElementPtr
                  ( "%" ^ varName ^ "_index_1"
                  , LocalVar (varName ^ "_snd")
                  , [IndexConst 0]
                  , typ )
              ; Store
                  ( LocalVar (varName ^ "_snd")
                  , typ1
                  , LocalVar (varName ^ "_index_1")
                  , typ ) ]
          in
          translateBlock e2 blks curr_blk2
      | AllocN (sz, value) ->
          (* Currently, the translation supports only sz == 1. *)
          let operand = translateExpr sz in
          let valu = translateExpr value in
          let typ1 = decide_ty value in
          let curr_blk1 =
            extend_blk curr_blk
              [ Call
                  (Some ("%" ^ varName), "malloc", [(operand, Int 64)], typ)
              ; GetElementPtr
                  ( "%" ^ varName ^ "_index_1"
                  , LocalVar varName
                  , [IndexConst 0]
                  , typ )
              ; Store (valu, typ1, LocalVar (varName ^ "_index_1"), typ) ]
          in
          translateBlock e2 blks curr_blk1
      | Var _ ->
          let operand = translateExpr e1 in
          let curr_blk1 =
            extend_blk curr_blk
              [ Alloca ("%" ^ varName, typ)
              ; Store (operand, decide_ty e1, LocalVar varName, Pointer typ)
              ]
          in
          translateBlock e2 blks curr_blk1
      | _ -> failwith "Translation not implemented for this expression" )
  | Var v -> blks @ [extend_blk curr_blk [Ret (Some (LocalVar v))]]
  | Val v -> blks @ [extend_blk curr_blk [Ret (Some (translateVal v))]]
  | If (cond, then_branch, else_branch) ->
      let cond_operand = translateExpr cond in
      let curr_blk' =
        extend_blk curr_blk [CondBr (cond_operand, "then", "else")]
      in
      let blks' = blks @ [curr_blk'] in
      let blks'' =
        translateBlock then_branch blks' {label= "then"; instructions= []}
      in
      let blks''' =
        translateBlock else_branch blks'' {label= "else"; instructions= []}
      in
      blks'''
  | _ -> failwith "Translation not implemented for this statement"

(* Add more translation cases as needed *)

and translateExpr e =
  match e with
  | Val v -> translateVal v
  | Var v -> LocalVar v
  | _ -> failwith "Translation not implemented for this expression"

(* Translate Heaplang values to LLVM IR operands *)

and translateVal v =
  match v with
  | LitV (LitInt i) -> ConstInt i
  | LitV (LitBool b) -> ConstInt (if b then 1 else 0)
  | LitV LitUnit -> ConstVoid
  | LitV (LitLoc l) -> ConstLoc l
  | LitV LitPoison -> failwith "Poison value not supported"
  | _ -> failwith "Translation not implemented for this value"
