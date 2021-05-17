open Util
open Primop

type var = string

type label = string

type atm =
    Int of int64
  | Var of var

type exp =
    Atom of atm
  | Prim of primop * atm list

type stmt =
    Assign of var * exp

type tail =
    Return of exp
  | Seq of stmt*tail

type 'a program = Program of 'a * (label*tail) list
    
(* Checking *)

let checkfail s = failwith ("CVar check failed: "^ s)

let check_atm env = function
  | Var x -> if not (Env.mem x env) then checkfail ("undefined  variable " ^ x)
  | _ -> ()

let check_exp env = function
  | Atom atm -> check_atm env atm
  | Prim (op,args) -> (List.iter (check_atm env) args;
		       check_primop op args)

let check_stmt env = function
    Assign (x,e) -> (check_exp env e; Env.add x () env)
    
let rec check_tail env = function
    Return e -> (check_exp env e; env)
  | Seq (s,t) -> let env' = check_stmt env s in
                 check_tail env' t
	
let check_program = function
    Program (_,[("start",t)]) -> Program(check_tail Env.empty t,[("start",t)])
  | _ -> checkfail "program is not a single tail labeled start"

(* Printing *)

open Printf

let print_atom oc = function
    Int i -> fprintf oc "%Ld" i
  | Var x -> fprintf oc "%s" x

let print_exp oc = function
    Atom(a) -> print_atom oc a
  | Prim(Read,_) -> output_string oc "(read)"
  | Prim(Neg,[a]) -> fprintf oc "(- %a)" print_atom a 
  | Prim(Add,[a1;a2]) -> fprintf oc "(+ %a %a)" print_atom a1 print_atom a2
  | _ -> assert false (* arity mismatch *)

let print_stmt oc = function
    Assign(x,e) -> fprintf oc "\t%s := %a\n" x print_exp e

let rec print_tail oc = function
    Return e -> fprintf oc "\treturn %a\n" print_exp e
  | Seq(s,t) ->
      print_stmt oc s;
      print_tail oc t

let print_labeled_tail oc (label,tail) =
  fprintf oc "%s:\n" label;
  print_tail oc tail

let print_tails oc tails = 
  List.iter (print_labeled_tail oc) tails

let print_program print_info oc (Program(info,tails)) =
  print_info oc info;
  print_tails oc tails


(* Interpretation *)

let interp_atm env = function
  | Int n -> n
  | Var x -> Env.find x env

let interp_exp env = function
    Atom a -> interp_atm env a
  | Prim(op,args) -> interp_primop op (List.map (interp_atm env) args)

let interp_stmt env = function
    Assign (v,e) -> Env.add v (interp_exp env e) env

let rec interp_tail env = function
    Return e -> interp_exp env e
  | Seq (s,t) -> interp_tail (interp_stmt env s) t

let interp_program = function
    Program (_,[("start",t)]) -> interp_tail Env.empty t
  | _ -> assert false
