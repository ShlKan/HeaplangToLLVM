type typ =
  | Void
  | Int of int (* e.g., i32, i64 *)
  | Float
  | Double
  | Pointer of typ
  | PointerNT
  | Array of int * typ
  | Struct of typ list
  | Function of typ list * typ
  | LVar of string
  | Pair of typ * typ (* Represents a pair type *)

type operand =
  | ConstVoid
  | ConstInt of int
  | ConstFloat of float
  | ConstLoc of int
  | ConstString of string
  | GlobalVar of string
  | LocalVar of string
  | Undef

type binop = Add | Sub | Mul | Div | And | Or | Xor | Rem

type gep_index = IndexConst of int | IndexVar of operand

type instruction =
  | BinOp of string * binop * operand * operand * typ
  | Icmp of string * string * operand * operand * typ (* Added for icmp *)
  | Load of string option * operand * typ
  | Store of operand * typ * operand * typ
  | Alloca of string * typ
  | Call of string option * operand * string * (operand * typ) list * typ
  | Ret of (operand * typ) option
  | Br of string
  | CondBr of operand * string * string
  | Phi of (operand * string) list * typ
  | GetElementPtr of
      string option * operand * gep_index list * typ (* GEP instruction *)
  | ExtractValue of
      string option * operand * int * typ (* Extract from a pair or struct *)
  | InsertValue of
      string
      * operand
      * (operand * typ)
      * int
      * typ (* Insert into a pair or struct *)
  | Assert of
      operand * string (* Assertion with a condition and error message *)
  | PtrToInt of string * operand * typ * typ

type basic_block = {label: string; instructions: instruction list}

type func =
  { name: string
  ; ret_type: typ
  ; params: (string * typ) list
  ; basic_blocks: basic_block list }

type global_var = {name: string; typ: typ; value: operand option}

type user_ty = {name: string; typ: typ}

type llvm_module =
  {globals: global_var list; user_typs: user_ty list; functions: func list}

let rec typ_to_string = function
  | Void -> "void"
  | Int n -> "i" ^ string_of_int n
  | LVar name -> "%" ^ name
  | Float -> "float"
  | Double -> "double"
  | PointerNT -> "ptr"
  | Pointer t -> typ_to_string t ^ "*"
  | Array (n, t) -> "[" ^ string_of_int n ^ " x " ^ typ_to_string t ^ "]"
  | Struct ts -> "{" ^ String.concat ", " (List.map typ_to_string ts) ^ "}"
  | Pair (t1, t2) -> "{" ^ typ_to_string t1 ^ ", " ^ typ_to_string t2 ^ "}"
  | Function (args, ret) ->
      typ_to_string ret ^ " ("
      ^ String.concat ", " (List.map typ_to_string args)
      ^ ")*"

let operand_to_string = function
  | ConstVoid -> "()"
  | ConstInt i -> string_of_int i
  | ConstFloat f -> string_of_float f
  | ConstLoc 0 -> "null"
  | ConstLoc l -> "loc(" ^ string_of_int l ^ ")"
  | ConstString s -> "\"" ^ s ^ "\""
  | GlobalVar name -> "@" ^ name
  | LocalVar name -> "%" ^ name
  | Undef -> "undef"

let arg_to_string arg =
  match arg with
  | _, Void -> ""
  | op, typ -> typ_to_string typ ^ " " ^ operand_to_string op

let binop_to_string = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "sdiv"
  | Rem -> "srem"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"

let gep_index_to_string = function
  | IndexConst i -> "i32 " ^ string_of_int i
  | IndexVar op -> "i32 " ^ operand_to_string op

let local_var name = "%" ^ name

let rec instruction_to_string = function
  | BinOp (name, op, lhs, rhs, typ) ->
      local_var name ^ " = " ^ binop_to_string op ^ " " ^ typ_to_string typ
      ^ " " ^ operand_to_string lhs ^ ", " ^ operand_to_string rhs
  | Load (Some name, op, typ) ->
      local_var name ^ " = load " ^ typ_to_string typ ^ ", "
      ^ arg_to_string (op, Pointer typ)
  | Load (None, op, typ) ->
      "load " ^ typ_to_string typ ^ ", " ^ arg_to_string (op, Pointer typ)
  | Store (src, typ1, dst, typ) ->
      "store " ^ typ_to_string typ1 ^ " " ^ operand_to_string src ^ ", "
      ^ typ_to_string typ ^ " " ^ operand_to_string dst
  | Alloca (name, typ) -> local_var name ^ " = alloca " ^ typ_to_string typ
  | Call (None, name, c_sig, args, typ) ->
      "call " ^ typ_to_string typ
      ^ (if c_sig = "" then "" else " " ^ c_sig)
      ^ " " ^ operand_to_string name ^ "("
      ^ String.concat ", " (List.map arg_to_string args)
      ^ ")"
  | Call (Some varName, name, c_sig, args, typ) ->
      if typ = Void then
        "call " ^ typ_to_string typ
        ^ (if c_sig = "" then "" else " " ^ c_sig)
        ^ " " ^ operand_to_string name ^ "("
        ^ String.concat ", " (List.map arg_to_string args)
        ^ ")"
      else
        local_var varName ^ " = call " ^ typ_to_string typ
        ^ (if c_sig = "" then "" else " " ^ c_sig)
        ^ " " ^ operand_to_string name ^ "("
        ^ String.concat ", " (List.map arg_to_string args)
        ^ ")"
  | Ret None -> "ret void"
  | Ret (Some (op, typ)) ->
      "ret " ^ typ_to_string typ ^ " " ^ operand_to_string op
  | Br label -> "br label %" ^ label
  | CondBr (cond, then_label, else_label) ->
      "br i1 " ^ operand_to_string cond ^ ", label %" ^ then_label
      ^ ", label %" ^ else_label
  | Phi (incoming, typ) ->
      "phi " ^ typ_to_string typ ^ " "
      ^ String.concat ", "
          (List.map
             (fun (op, label) ->
               "[" ^ operand_to_string op ^ ", %" ^ label ^ "]" )
             incoming )
  | GetElementPtr (name, base, indices, typ) -> (
    match name with
    | None ->
        "getelementptr " ^ typ_to_string typ ^ ", "
        ^ typ_to_string (Pointer typ)
        ^ ", " ^ operand_to_string base ^ ", "
        ^ String.concat ", " (List.map gep_index_to_string indices)
    | Some name ->
        local_var name ^ " = getelementptr " ^ typ_to_string typ ^ ", "
        ^ typ_to_string (Pointer typ)
        ^ " " ^ operand_to_string base ^ ", "
        ^ String.concat ", " (List.map gep_index_to_string indices) )
  | ExtractValue (name, op, idx, typ) -> (
    match name with
    | None ->
        "extractvalue " ^ typ_to_string typ ^ " " ^ operand_to_string op
        ^ ", " ^ string_of_int idx
    | Some name ->
        local_var name ^ " = extractvalue " ^ typ_to_string typ ^ " "
        ^ operand_to_string op ^ ", " ^ string_of_int idx )
  | InsertValue (name, agg, val_op, idx, typ) ->
      local_var name ^ " = insertvalue " ^ typ_to_string typ ^ " "
      ^ operand_to_string agg ^ ", "
      ^ typ_to_string (snd val_op)
      ^ " "
      ^ operand_to_string (fst val_op)
      ^ ", " ^ string_of_int idx
  | Icmp (name, cond, lhs, rhs, typ) ->
      local_var name ^ " = icmp " ^ cond ^ " " ^ typ_to_string typ ^ " "
      ^ operand_to_string lhs ^ ", " ^ operand_to_string rhs
  | Assert (cond, msg) ->
      "assert " ^ operand_to_string cond ^ " \"" ^ msg ^ "\""
  | PtrToInt (name, op, from_typ, to_typ) ->
      local_var name ^ " = ptrtoint " ^ typ_to_string from_typ ^ " "
      ^ operand_to_string op ^ " to " ^ typ_to_string to_typ

let basic_block_to_string {label; instructions} =
  label ^ ":\n"
  ^ String.concat "\n"
      (List.map
         (fun instr -> "  " ^ instruction_to_string instr)
         instructions )

let func_to_string {name; ret_type; params; basic_blocks} =
  "define " ^ typ_to_string ret_type ^ " @" ^ name ^ "("
  ^ String.concat ", "
      (List.map
         (fun (pname, ptyp) -> typ_to_string ptyp ^ " %" ^ pname)
         params )
  ^ ") {\n"
  ^ String.concat "\n" (List.map basic_block_to_string basic_blocks)
  ^ "\n}"

let global_var_to_string {name; typ; value} =
  "@" ^ name ^ " = global " ^ typ_to_string typ
  ^ match value with None -> "" | Some v -> " " ^ operand_to_string v

let llvm_module_to_string {globals; user_typs; functions} =
  let user_typs_str =
    String.concat "\n"
      (List.map
         (fun {name; typ} -> "%" ^ name ^ " = type " ^ typ_to_string typ)
         user_typs )
  in
  let globals_str =
    String.concat "\n" (List.map global_var_to_string globals)
  in
  let functions_str =
    String.concat "\n\n" (List.map func_to_string functions)
  in
  String.concat "\n\n"
    (List.filter
       (fun s -> s <> "")
       [user_typs_str; globals_str; functions_str] )

let print_llvm_module m = print_endline (llvm_module_to_string m)

let instructions_to_string instructions =
  String.concat "\n" (List.map instruction_to_string instructions)
