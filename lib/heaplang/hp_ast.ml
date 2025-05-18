type un_op = NegOp | MinusUnOp

type bin_op =
  | PlusOp
  | MinusOp
  | MultOp
  | QuotOp
  | RemOp
  | AndOp
  | OrOp
  | XorOp
  | ShiftLOp
  | ShiftROp
  | LeOp
  | LtOp
  | EqOp
  | OffsetOp

type base_lit =
  | LitInt of int
  | LitBool of bool
  | LitUnit
  | LitPoison
  | LitLoc of int

(* Heaplang itself is not explicitly typed. In order to generate LLVM IR, we
   need instantiate the function signature with types. *)
type htyp =
  | TUnit
  | TBool
  | TInt
  | TLoc
  | TFun of htyp list
  | TPair of (htyp * htyp)

let rec htyp_to_string = function
  | TUnit -> "unit"
  | TBool -> "bool"
  | TInt -> "int"
  | TLoc -> "loc"
  | TFun args ->
      "fun(" ^ String.concat ", " (List.map htyp_to_string args) ^ ")"
  | TPair (t1, t2) ->
      "(" ^ htyp_to_string t1 ^ " * " ^ htyp_to_string t2 ^ ")"

let htyp_list_to_string htyp_list =
  "[" ^ String.concat "; " (List.map htyp_to_string htyp_list) ^ "]"

type binder = BAnon | BNamed of string | BTyped of (string * htyp)

type value =
  | LitV of base_lit
  | PairV of (value * value)
  | InjLV of value
  | InjRV of value

type expr =
  | Val of value
  | Var of string
  | Rec of (binder * binder * expr * htyp)
  | App of (expr * expr)
  | UnOp of (un_op * expr)
  | BinOp of (bin_op * expr * expr)
  | If of (expr * expr * expr)
  | Pair of (expr * expr)
  | Fst of expr
  | Snd of expr
  | InjL of expr
  | InjR of expr
  | Case of (expr * expr * expr)
  | AllocN of (expr * expr)
  | Free of expr
  | Load of expr
  | Store of (expr * expr)
  | CmpXchg of (expr * expr * expr)
  | Xchg of (expr * expr)
  | FAA of (expr * expr)
  | Fork of expr
  | NewProph of expr
  | Resolve of (expr * expr * expr)
  | Let of (expr * expr * expr)
  | Print of expr
  | Seq of (expr * expr)

let rec ast_to_string = function
  | Val (LitV (LitInt i)) -> string_of_int i
  | Val (LitV (LitBool b)) -> string_of_bool b
  | Val (LitV LitUnit) -> "()"
  | Val (LitV LitPoison) -> "poison"
  | Val (LitV (LitLoc l)) -> "loc(" ^ string_of_int l ^ ")"
  | Val (PairV (v1, v2)) ->
      "(" ^ ast_to_string (Val v1) ^ ", " ^ ast_to_string (Val v2) ^ ")"
  | Val (InjLV v) -> "inl(" ^ ast_to_string (Val v) ^ ")"
  | Val (InjRV v) -> "inr(" ^ ast_to_string (Val v) ^ ")"
  | Var v -> v
  | Rec (BAnon, BAnon, body, typ) ->
      "rec _ : _ " ^ "->" ^ htyp_to_string typ ^ ast_to_string body
  | Rec (BAnon, BNamed f, body, typ) ->
      "rec _ : " ^ f ^ " -> " ^ htyp_to_string typ ^ ast_to_string body
  | Rec (BNamed x, BNamed f, body, typ) ->
      "rec " ^ x ^ " : " ^ f ^ "->" ^ htyp_to_string typ ^ ast_to_string body
  | App (e1, e2) -> "(" ^ ast_to_string e1 ^ " " ^ ast_to_string e2 ^ ")"
  | UnOp (op, e) ->
      "unop(" ^ un_op_to_string op ^ ", " ^ ast_to_string e ^ ")"
  | BinOp (op, e1, e2) ->
      "binop(" ^ bin_op_to_string op ^ ", " ^ ast_to_string e1 ^ ", "
      ^ ast_to_string e2 ^ ")"
  | If (e1, e2, e3) ->
      "if " ^ ast_to_string e1 ^ " then " ^ ast_to_string e2 ^ " else "
      ^ ast_to_string e3
  | Pair (e1, e2) -> "(" ^ ast_to_string e1 ^ ", " ^ ast_to_string e2 ^ ")"
  | Fst e -> "fst(" ^ ast_to_string e ^ ")"
  | Snd e -> "snd(" ^ ast_to_string e ^ ")"
  | InjL e -> "inl(" ^ ast_to_string e ^ ")"
  | InjR e -> "inr(" ^ ast_to_string e ^ ")"
  | Case (e, e1, e2) ->
      "case " ^ ast_to_string e ^ " of " ^ ast_to_string e1 ^ " | "
      ^ ast_to_string e2
  | AllocN (e1, e2) ->
      "allocN(" ^ ast_to_string e1 ^ ", " ^ ast_to_string e2 ^ ")"
  | Free e -> "free(" ^ ast_to_string e ^ ")"
  | Load e -> "load(" ^ ast_to_string e ^ ")"
  | Store (e1, e2) ->
      "store(" ^ ast_to_string e1 ^ ", " ^ ast_to_string e2 ^ ")"
  | CmpXchg (e1, e2, e3) ->
      "cmpxchg(" ^ ast_to_string e1 ^ ", " ^ ast_to_string e2 ^ ", "
      ^ ast_to_string e3 ^ ")"
  | Xchg (e1, e2) ->
      "xchg(" ^ ast_to_string e1 ^ ", " ^ ast_to_string e2 ^ ")"
  | FAA (e1, e2) -> "faa(" ^ ast_to_string e1 ^ ", " ^ ast_to_string e2 ^ ")"
  | Fork e -> "fork(" ^ ast_to_string e ^ ")"
  | NewProph e -> "new_proph(" ^ ast_to_string e ^ ")"
  | Resolve (e1, e2, e3) ->
      "resolve(" ^ ast_to_string e1 ^ ", " ^ ast_to_string e2 ^ ", "
      ^ ast_to_string e3 ^ ")"
  | Let (e1, e2, e3) ->
      "let " ^ ast_to_string e1 ^ " = " ^ ast_to_string e2 ^ " in "
      ^ ast_to_string e3
  | Print e -> "print(" ^ ast_to_string e ^ ")"
  | Seq (e1, e2) -> ast_to_string e1 ^ ";; " ^ ast_to_string e2
  | _ -> "unknown"

and un_op_to_string = function NegOp -> "neg" | MinusUnOp -> "-"

and bin_op_to_string = function
  | PlusOp -> "+"
  | MinusOp -> "-"
  | MultOp -> "*"
  | QuotOp -> "/"
  | RemOp -> "%"
  | AndOp -> "&&"
  | OrOp -> "||"
  | XorOp -> "^"
  | ShiftLOp -> "<<"
  | ShiftROp -> ">>"
  | LeOp -> "<="
  | LtOp -> "<"
  | EqOp -> "=="
  | OffsetOp -> "offset"
