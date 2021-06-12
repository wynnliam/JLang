(* Primop handling, factored out of source language and some intermediate languages. *)

open Util

type comparison =
  | LT (* Less than *)
  | LE (* Less than or equal *)
  | EQ (* Equal *)
  | GE (* Greater than or equal *)
  | GT (* Greater than *)

type primop =
  | Read
  | Print
  | Neg
  | Add
  | Compare of comparison

let arity = function
  | Read -> 0
  | Print -> 1
  | Neg -> 1
  | Add -> 2
  | Compare _ -> 2

let string_of_comparison = function
  | LT -> "<"
  | LE -> "<="
  | EQ -> "="
  | GE -> ">="
  | GT -> ">"

let string_of_primop = function
  | Read -> "read"
  | Print -> "print"
  | Neg -> "-"
  | Add -> "+"
  | Compare c -> string_of_comparison c

let checkfail s = failwith ("Primop check failure: " ^ s)

let check_primop (op:primop) (args:'a list) =
  let expected = arity op in
  let actual = List.length args in
  if expected <> actual then
    checkfail ("primop " ^ (string_of_primop op) ^ " expects " ^ (string_of_int expected)  ^ " args but got " ^ (string_of_int actual))

let interp_primop (op:primop) (args: value list) : value = 
  match op,args with
    Read,[] -> read_int()
  | Neg,[v] -> Int32.neg v
  | Add,[v1;v2] -> Int32.add v1 v2
  | _,_ -> assert false  (* arity mismatch *)
