open Util

module Uniquify =
  struct
    open JVar

    let rec do_exp env = function
	Var x ->
	  Var (Env.find x env)
      | Int n ->
	  Int n
      | Let (x,e1,e2) ->
	  let x' = gensym x in
	  Let(x', do_exp env e1, do_exp (Env.add x x' env) e2)
      | Prim (primop,args) ->
	 Prim(primop, List.map (do_exp env) args)
	    
    let do_program (Program(a,e)) =
      fresh := 0;
      Program(a,do_exp Env.empty e)

    (* checking: all let bindings should be unique within program *)
    let envr = ref Env.empty

    let rec check_exp  = function
      | Let(x,e1,e2) ->
	  if Env.mem x (!envr) then 
	    (Printf.eprintf "multiple bindings of %s\n%!" x;
	     failwith "Uniquify check failed");
	  envr := Env.add x () (!envr);
	  check_exp e1;
	  check_exp e2
      | Prim(_,args) -> List.iter check_exp args
      | _ -> ()

    let check_program (Program(_,e) as p) =
      envr := Env.empty;
      check_exp e;
      JVar.check_program false p

    let pass : (unit JVar.program, unit JVar.program, unit JVar.program) pass =
      {name="uniquify";
       transformer=do_program;
       printer=print_program;
       checker=check_program;}
  end (* Uniquify *) 

module RemoveComplexOperands = 
  struct
    open JVar

    (* Behold this monstrosity I did before! *)
    (* let rec rco_atom env e =
    match e with
    | Prim (Neg, [e]) ->
      let s = gensym "`tmp" in [(s, rco_exp env e)]
    | Prim (Add, [Int n1; e]) ->
      let s = gensym "`tmp" in [(s, rco_exp env e)]
    | Prim (Add, [e; Int n2]) ->
      let s = gensym "`tmp" in [(s, rco_exp env e)]
    | Prim (Add, [e1; e2]) ->
      let s1 = gensym "`tmp" in
      let s2 = gensym "`tmp" in
      [(s1, rco_exp env e1); (s2, rco_exp env e2)]
    | e -> let s = gensym "`tmp" in [(s, e)]

    and rco_exp env e = e
      match e with
      | Int n -> Int n
      | Var v -> Var v
      | Prim (Read, []) -> Prim (Read, [])

      | Prim (Neg, [Int n]) -> Prim (Neg, [Int n])
      | Prim (Neg, [Var v]) -> Prim (Neg, [Var v])
      | Prim (Neg, [cmplx]) ->
        let [(name, exp)] = rco_atom env e in
        Let (name, exp, Prim (Neg, [Var name]))

      | Prim (Add, [Int n1; Int n2]) -> Prim (Add, [Int n1; Int n2])
      | Prim (Add, [Int n1; Var v2]) -> Prim (Add, [Int n1; Var v2])
      | Prim (Add, [Var v1; Int n2]) -> Prim (Add, [Var v1; Int n2])
      | Prim (Add, [Var v1; Var v2]) -> Prim (Add, [Var v1; Var v2])
      | Prim (Add, [Int n1; cmplx]) ->
        let [(name, exp)] = rco_atom env e in
        Let (name, exp, Prim (Add, [Int n1; Var name]))
      | Prim (Add, [cmplx; Int n2]) ->
        let [(name, exp)] = rco_atom env e in
        Let (name, exp, Prim (Add, [Var name; Int n2]))
      | Prim (Add, [cmplx1; cmplx2]) ->
        let [(name1, exp1); (name2, exp2)] = rco_atom env e in
        Let (name1, exp1, Let (name2, exp2, Prim (Add, [Var name1; Var name2])))
      | exp -> exp*)

    let is_atm exp =
    match exp with
    | Int n -> true
    | Var v -> true
    | _ -> false

    let rec rco_atm  exp =
      match exp with
      | Prim (Read, []) ->
        let s = gensym "`tmp" in (s, Prim (Read, []))
      | Prim (Neg, [e]) ->
        let s = gensym "`tmp" in (s, rco_exp  (Prim (Neg, [e])))
      | Prim (Add, [e1; e2]) ->
        let s = gensym "`tmp" in (s, rco_exp  (Prim (Add, [e1; e2])))
      | Let (x, e1, e2) ->
        let s = gensym "`tmp" in (s, rco_exp (Let (x, e1, e2)))
      | _ -> failwith "Unhandled case of atomization"

    and rco_exp exp =
      match exp with
      | Let (x, e1, e2) -> Let (x, rco_exp  e1, rco_exp  e2)
      | Prim (Neg, [e]) ->
        if is_atm e then Prim (Neg, [e])
        else let (name, result) = rco_atm  e in
        Let (name, result, Prim (Neg, [Var name]))
      | Prim (Read, []) -> Prim (Read, [])
      | Prim (Add, [e1; e2]) -> rco_add  (Prim (Add, [e1; e2]))
      | _ -> exp

    and rco_add command =
      match command with
      | Prim (Add, [e1; e2]) ->
          (* Note: Tried to do this as a match expression, but they don't work
             to well if put inside another match expression *)
          if (is_atm e1) && (is_atm e2) then Prim (Add, [e1; e2])
          else if (is_atm e1) then let (name, result) = rco_atm  e2 in
                                   Let (name, result, Prim (Add, [e1; Var name]))
          else if (is_atm e2) then let (name, result) = rco_atm  e1 in
                                   Let (name, result, Prim (Add, [Var name; e2]))
          else let (n1, r1) = rco_atm  e1 in
               let (n2, r2) = rco_atm  e2 in
               Let (n1, r1, Let (n2, r2, Prim (Add, [Var n1; Var n2])))
      | _ -> failwith "Error: bad command given!"

    let do_program (Program(a,e)) =
      fresh := 0;
      Program(a, rco_exp e)

    let exp_iter_with_fail (exp : JVar.exp) =
      if is_atm exp then ()
      else failwith "Non-atom prim ops found"

    let rec check_exp (exp : JVar.exp) =
    match exp with
    | Let (x, e1, e2) -> check_exp e1; check_exp e2
    | Prim (Read, []) -> ()
    | Prim (_, exps) -> List.iter exp_iter_with_fail exps
    | _ -> ()

    let check_program (Program(_,e) as p) =
      check_exp e;
      JVar.check_program false p

    let pass : (unit JVar.program, unit JVar.program, unit JVar.program) pass =
      {name="remove complex operands";
       transformer=do_program;
       printer=print_program;
       checker=check_program;}
  end (* RemoveComplexOperands *)

(*let get_exp (JVar.Program(a, e)) = e
let test str = RemoveComplexOperands.rco_exp Util.Env.empty (get_exp (JVar.parse_from_string str));*)
    
module ExplicateControl =
  struct 
    open CVar

    let convert_to_catm ratm =
      match ratm with
      | JVar.Int n -> Int n
      | JVar.Var v -> Var v
      | _ -> failwith "Trying to convert non-atom!"

    (* Requires that texp is in tail position *)
    let rec explicate_tail texp =
      match texp with
      | JVar.Int n -> Return (Atom (Int n))
      | JVar.Var v -> Return (Atom (Var v))
      | JVar.Prim (Neg, [atm]) -> Return (Prim (Neg, [convert_to_catm atm]))
      | JVar.Prim (Add, [atm1;atm2]) -> Return (Prim (Add, [convert_to_catm atm1; convert_to_catm atm2]))
      | JVar.Prim (Read, []) -> Return (Prim (Read, []))
      | JVar.Let (x, e1, e2) ->
        let result = explicate_tail e2 in
        explicate_assign e1 x result
      | _ -> failwith "Unhandled explicate_tail case!"

    and explicate_assign exp x cont =
      match exp with
      | JVar.Int n -> Seq (Assign (x, Atom (Int n)), cont)
      | JVar.Var v -> Seq (Assign (x, Atom (Var v)), cont)
      | JVar.Prim (Neg, [atm]) ->
        let c = convert_to_catm atm in
        let n = Prim (Neg, [c]) in
        Seq (Assign (x, n), cont)
      | JVar.Prim (Add, [atm1; atm2]) ->
        let c1 = convert_to_catm atm1 in
        let c2 = convert_to_catm atm2 in
        let a = Prim (Add, [c1; c2]) in
        Seq (Assign (x, a), cont)
      | JVar.Prim (Read, []) -> Seq (Assign (x, Prim (Read, [])), cont)
      | JVar.Let (y, rhs, body) ->
        let e1 = explicate_assign body x cont in
        explicate_assign rhs y e1
      | _ -> failwith "Unhandled explicate_assign case!"

    let do_program (JVar.Program (_, exp)) =
      Program ((), [("start", explicate_tail exp)])

    let pass : (unit JVar.program, unit CVar.program, (unit Env.t) CVar.program) pass =
      {name="explicate control";
       transformer=do_program;
       printer=print_program (fun _ _ -> ());
       checker=check_program;}

  end (* ExplicateControl *)

module SelectInstructions =
  struct
    open JasmInt

(*
type primop =
    Read
  | Neg
  | Add

let arity = function
    Read -> 0
  | Neg -> 1
  | Add -> 2

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
  | Seq of stmt*tail*)

    let do_atm a = assert false

    let do_exp (exp : CVar.exp) =
      match exp with
      | Atom a -> do_atm a
      | Prim(op, args) -> failwith "Not yet implemented!"

    let do_stmt s = assert false

    let rec do_tail (tail : CVar.tail) =
      match tail with
      | Return exp -> do_exp exp
      | Seq(s, t) -> (do_stmt s) @ (do_tail t)

    let do_program (CVar.Program(vars, lbs)) =
      let f = fun acc (lbl, t) -> acc @ (do_tail t) in
      let b = List.fold_left f [] lbs in
      Program((), "main", b)

    let check_program (Program (pinfo, lbl, instrs)) =
      let p = Env.empty in
      Program(p, lbl, instrs)

    (* TODO: Implement pass *)
    let pass : ((unit Env.t) CVar.program,
                unit JasmInt.program,
                (int Env.t) JasmInt.program) pass =
    {name="select instructions";
     transformer=do_program;
     printer=print_program;
     checker=check_program;}

  end (* Select Instructions *)

(* This pass is always required: it does static checking on the source and can be used to obtain the
   "correct" result of evaluation. *)
let initial_pass : (unit JVar.program, unit JVar.program, unit JVar.program) pass = 
  {name="source checking";
   transformer=(fun p -> p);
   printer=JVar.print_program;	      
   checker=JVar.check_program true;}

(* This final pass is suitable for testing the generated X86 code "for real." *)
let execute_pass : ((int,unit) X86Int.program, (int,unit) X86Int.program, (int,unit) X86Int.program) pass =  
  {name="emit, assemble, link, run";
   transformer=(fun p -> p);
   printer=(fun oc p -> ());
   checker=(fun p -> p);}

(* An alternative final pass that just emits assembly code to a .s file *)
let emit_pass : ((int,unit) X86Int.program, (int,unit) X86Int.program, (int,unit) X86Int.program) pass =  
  {name="emit";
   transformer=(fun p -> p);
   printer=(fun oc p -> ());
   checker=(fun p -> p);}

(* Define sequence of passes for this Chapter.
   Adjust this as you implement more passes.
 *)
let passes = 
     PCons(initial_pass,
     PCons(Uniquify.pass,
     PCons(RemoveComplexOperands.pass,
     PCons(ExplicateControl.pass,
     PCons(SelectInstructions.pass,
     PNil)))))
     (*PCons(SelectInstructions.pass,
     PCons(AssignHomes.pass,
     PCons(PatchInstructions.pass,
     PCons(execute_pass,
	   PNil))))))))*)
   
(* Some specializations of Util functions.
   You may wish to alter the initial boolean flags. 
   You may also wish to adjust the Util.debug_level flag *)

let _ = Util.debug_level := 0

(* Suitable for top-level use. *)
let test_string = Util.test_string true false JVar.parse_from_string passes 

(* Suitable for bulk testing with comparison between first and last pass results. *)
let test_files = Util.test_files false true JVar.parse_from_channel passes 

(* Packaging interpreter as a command line executable.
   Run this, as, e.g. 
     driver [-d debug-level] tests/int*.r
*)
let _ = if not !Sys.interactive then 
 (let usage_msg = "driver [-d debug-level] file.r ..." in
  let input_files = ref [] in
  let speclist = [("-d",Arg.Set_int Util.debug_level,"Set debug level")] in
  let anon_fun filename = input_files := filename::!input_files in
  Arg.parse speclist anon_fun usage_msg;
  test_files (List.rev (!input_files));
  exit(0))

(* If you build a top-level driver.top for interactive use, you'll need to open
   any Chapter2 and other modules in order to access the functions in these files. *)
