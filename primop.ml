(* Primop handling, factored out of source language and some intermediate languages. *)

open Util

type primop =
    Read
  | Neg
  | Add

let arity = function
    Read -> 0
  | Neg -> 1
  | Add -> 2

let string_of_primop = function
    Read -> "read"
  | Neg -> "-"
  | Add -> "+"

let checkfail s = failwith ("Primop check failure: " ^ s)

let check_primop (op:primop) (args:'a list) =
  let expected = arity op in
  let actual = List.length args in
  if expected <> actual then
    checkfail ("primop " ^ (string_of_primop op) ^ " expects " ^ (string_of_int expected)  ^ " args but got " ^ (string_of_int actual))

let interp_primop (op:primop) (args: value list) : value = 
  match op,args with
    Read,[] -> read_int()
  | Neg,[v] -> Int64.neg v
  | Add,[v1;v2] -> Int64.add v1 v2
  | _,_ -> assert false  (* arity mismatch *)
