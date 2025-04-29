type typ =
  | Void
  | Int of int  (* e.g., i32, i64 *)
  | Float
  | Double
  | Pointer of typ
  | Array of int * typ
  | Struct of typ list
  | Pair of typ * typ  (* Represents a pair type *)

type operand =
  | ConstInt of int
  | ConstFloat of float
  | ConstString of string
  | GlobalVar of string
  | LocalVar of string

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Xor

type gep_index =
  | IndexConst of int
  | IndexVar of operand

type instruction =
  | BinOp of binop * operand * operand * typ
  | Load of operand * typ
  | Store of operand * operand * typ
  | Alloca of typ
  | Call of string * operand list * typ
  | Ret of operand option
  | Br of string
  | CondBr of operand * string * string
  | Phi of (operand * string) list * typ
  | GetElementPtr of operand * gep_index list * typ  (* GEP instruction *)
  | ExtractValue of operand * int * typ             (* Extract from a pair or struct *)
  | InsertValue of operand * operand * int * typ    (* Insert into a pair or struct *)
  | Assert of operand * string                      (* Assertion with a condition and error message *)

type basic_block = {
  label: string;
  instructions: instruction list;
}

type func = {
  name: string;
  ret_type: typ;
  params: (string * typ) list;
  basic_blocks: basic_block list;
}

type global_var = {
  name: string;
  typ: typ;
  value: operand option;
}

type llvm_module = {
  globals: global_var list;
  functions: func list;
}