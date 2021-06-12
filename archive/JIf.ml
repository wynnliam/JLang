open Util
open Primop

type var = string

type exp =
  | Int of int32
  | Prim of primop * exp list
  | Var of var
  | Let of var * exp * exp
    (* Value is var after assign *)
  | Assign of var * exp
  | Seq of exp list
  | If of exp * exp * exp

type 'info program = Program of 'info * exp

(* Conversion from/to Sexpressions *)
module Convert =
struct
  open Sexpr

  let rec parse_exp = function 
    | SNum i -> Int i
    | SSym x -> Var x
    | SList[SSym "let";SSym(x);e1;e2] -> Let(x,parse_exp e1, parse_exp e2)
    | SList[SSym "read"] -> Prim(Read,[])
    | SList[SSym "-";e] -> Prim(Neg,[parse_exp e])
    | SList[SSym "+";e1;e2] -> Prim(Add,[parse_exp e1; parse_exp e2])
    | SList[SSym "-";e1;e2] -> Prim(Add,[parse_exp e1; Prim(Neg,[parse_exp e2])])  (* "syntactic sugar" *)
    | SList[SSym "print";e] -> Prim(Print, [parse_exp e])
    | SList[SSym "<";e1;e2] -> Prim(Compare LT,  [parse_exp e1; parse_exp e2])
    | SList[SSym "<=";e1;e2] -> Prim(Compare LE, [parse_exp e1; parse_exp e2])
    | SList[SSym "=";e1;e2] -> Prim(Compare EQ,  [parse_exp e1; parse_exp e2])
    | SList[SSym "!=";e1;e2] -> Prim(Compare NE, [parse_exp e1; parse_exp e2])
    | SList[SSym ">=";e1;e2] -> Prim(Compare GE, [parse_exp e1; parse_exp e2])
    | SList[SSym ">";e1;e2] -> Prim(Compare GT,  [parse_exp e1; parse_exp e2])
    | SList[SSym ":=";SSym(x);e2] -> Assign(x, parse_exp e2)
    | SList(SSym "seq" :: es) -> Seq(List.map parse_exp es)
    | SList[SSym "if";e1;e2;e3] -> If(parse_exp e1, parse_exp e2, parse_exp e3)
    | sexp -> (prerr_endline "Cannot parse expression:";
	       Print.print stderr sexp;
	       prerr_newline();
	       raise Util.ParseError)
	  
  let parse_program sexp = Program((),parse_exp sexp)

  let rec print_exp = function
      Int i -> SNum i
    | Var x -> SSym x
    | Let (x,e1,e2) -> SList[SSym "let"; SSym(x); print_exp e1; print_exp e2]
    | Prim(Read,_) -> SList[SSym "read"]
    | Prim(Neg,[e]) -> SList[SSym "-"; print_exp e]
    | Prim(Add,[e1;e2]) -> SList[SSym "+"; print_exp e1; print_exp e2]
    | Prim(Print, [e]) -> SList[SSym "print"; print_exp e]
    | Prim(Compare c, [e1;e2]) -> SList[SSym (string_of_comparison c); print_exp e1; print_exp e2]
    | Assign(x,e) -> SList[SSym ":="; SSym x; print_exp e]
    | Seq es -> SList (List.map print_exp es)
    | If(cnd, thn, els) -> SList[SSym "if"; print_exp cnd; print_exp thn; print_exp els]
    (* there is no reliable way to "re-sugar" the subtraction operator *)
    | _ -> assert false
	  
  let print_program (Program(_,exp)) = print_exp exp

end  (* Convert *)

(* Parsing *)

(* Combining S-expression processing and conversion *)
let parse_from_channel ic =
  (try Sexpr.parse_from_channel ic
  with Sexpr.ParseError -> raise ParseError)
  |> Convert.parse_program

let parse_from_string s = 
  (try Sexpr.parse_from_string s
  with Sexpr.ParseError -> raise ParseError)
  |> Convert.parse_program 

let print_program oc p =
  Convert.print_program p 
  |> Sexpr.print oc

let print_expr oc e =
  Convert.print_exp e
  |> Sexpr.print oc

(* Static Checking *)

exception CheckError

let rec check_exp (env: unit Env.t) = function
     Int _ -> ()
   | Prim (op,args) -> (List.iter (check_exp env) args;
                        check_primop op args)
   | Var x -> if not (Env.mem x env) then
               (Printf.eprintf "Checking error: unbound variable %s\n" x; flush stderr;
                raise CheckError)
   | Let (x,e1,e2) -> (check_exp env e1; check_exp (Env.add x () env) e2)
   | Assign(x,e) ->
      if not (Env.mem x env) then
        (Printf.eprintf "Checking error: unbound variable %s\n" x; flush stderr;
         raise CheckError)
      else
        check_exp env e
  | Seq es -> List.iter (check_exp env) es; ()
  | If(cnd,thn,els) -> check_exp env cnd; check_exp env thn; check_exp env els; ()
	
(* 'user' argument should be true if checking an initial program, where errors are user errors;
   later on, it should be false, since new errors are must be due to compiler internal errors. *) 
let check_program (user:bool)  (Program (_,e) as p) =
   try 
     check_exp Env.empty e;
     p
   with
     CheckError -> if user then raise UserStaticError else failwith "JVar check failure"
   
(* Interpretation *)

let rec interp_exp (env:value Env.t) = function
    Int n -> n
  | Prim(op,args) -> interp_primop op (List.map (interp_exp env) args)
  | Var x -> Env.find x env
  | Let (x,e1,e2) -> interp_exp (Env.add x (interp_exp env e1) env) e2

let interp_program (Program(_,e)) = interp_exp Env.empty e


	
