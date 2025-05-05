open Heaplang
open Llvmir
open Hp_ast
open Llvm_ast

(* This module translates Heaplang expressions into LLVM IR. *)

let decide_ty e =
  match e with
  | Val (LitV (LitInt _)) -> Int 32
  | Val (LitV (LitBool _)) -> Int 1
  | Val (LitV LitUnit) -> Void
  | _ -> failwith "Type not supported in translation"

let var_name = function Var v -> v | _ -> failwith "Expected a variable"

(* Translate a Let binding *)

let rec translate exp =
  match exp with
  | Let (x, e1, e2) ->
      let varName = var_name x in
      let typ = decide_ty e1 in
      let operand = translateExpr e1 in
      Alloca (varName, typ)
      :: Store (LocalVar varName, operand, typ)
      :: translate e2
  | Var v -> [Ret (Some (LocalVar v))]
  | _ -> failwith "Translation not implemented for this expression"

(* Add more translation cases as needed *)

and translateExpr e =
  match e with
  | Val v -> translateVal v
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
