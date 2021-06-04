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
      | Assign(x, e) -> Assign(Env.find x env, do_exp env e)
      | Print x -> Print (Env.find x env)
	    
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

module EmitJasm =
  struct
    open JasmInt
    let do_op op =
      match op with
      | Primop.Add -> [Add]
      | Primop.Neg -> [Neg]
      | Primop.Read -> [InvokeStatic readn]

    let rec do_exp exp =
      match exp with
      | JVar.Int i -> [Push (Imm i)]
      | JVar.Var v -> [Load (Var v)]
      | JVar.Prim(op, args) ->
          let f = fun acc e -> (do_exp e) @ acc in
          let args' = List.fold_left f [] args in
          let op' = do_op op in
          args' @ op'
      | JVar.Let(v, e1, e2) ->
          let e1' = do_exp e1 in
          let v' = [Store(Var v)] in
          let e2' = do_exp e2 in
          e1' @ v' @ e2'
      | JVar.Assign(v, e) ->
          let e' = do_exp e in
          e' @ [Store(Var v)]
      | JVar.Print r -> [Store (Var r); Load (Var r); InvokeStatic writn]
        

    let emit_jasm expr =
      let instrs = do_exp expr in
      let r = gensym "`result" in
      let print_instrs =
        [Store (Var r); Load (Var r); InvokeStatic writn] in
      instrs (*@ print_instrs*)

    let do_program (JVar.Program(pinfo, expr)) =
      Program((), "main", emit_jasm expr)

    let env = ref Env.empty

    let var_exists (v : string) (env : unit Env.t) =
      match (Env.find_opt v env) with
      | Some i -> ()
      | None -> failwith "Variable not found!"
      
    let check_instr (instr : instr) =
      match instr with
      | Push (Var v) -> failwith "Cannot push Vars. Only Imms!"
      | Load (Var v) ->
          var_exists v !env;
          instr
      | Store (Var v) ->
          env := Env.add v () !env;
          instr
      | _ -> instr

    let check_program (Program (pinfo, lbl, instrs)) =
      let instrs' = List.map check_instr instrs in
      Util.print_env (fun _ _ -> ()) stdout !env;
      Program(!env, lbl, instrs')

    let pass : (unit JVar.program, unit JasmInt.program, unit Util.Env.t JasmInt.program) pass =
      {name="emit jasm";
       transformer=do_program;
       printer=print_program;
       checker=check_program;}
  end

module ChooseVariableIndex =
  struct
    open JasmInt
    let get_index env var = Int64.of_int (Env.find var env)

    let do_instr env instr = 
      match instr with
      | Push (Var v) -> Push (Imm (get_index env v))
      | Load (Var v) -> Load (Imm (get_index env v))
      | Store (Var v) -> Store (Imm (get_index env v))
      | _ -> instr

    let do_program (Program(env, lbl, instrs)) =
      (* Also tells us how many locals we have *)
      let ind = ref 1 in
      let f = fun _ ->
        let r = !ind in
        ind := !ind + 1;
        r in
      let ind_env = Env.map f env in
      Program(!ind, lbl, List.map (do_instr ind_env) instrs)

    let check_instr (instr : instr) =
      match instr with
      | Push (Var v) -> failwith "Found a Var. Should be an int!"
      | Load (Var v) -> failwith "Found a Var. Should be an int!"
      | Store (Var v) -> failwith "Found a Var. Should be an int!"
      | _ -> ()

    let check_program (Program (pinfo, lbl, instrs)) =
      List.iter check_instr instrs;
      Printf.fprintf stdout "num locals: %d\n" pinfo;
      Program(pinfo, lbl, instrs)

    let pass : ((unit Env.t) JasmInt.program,
                int JasmInt.program,
                int JasmInt.program) pass =
    {name="choose variable index";
     transformer=do_program;
     printer=print_program;
     checker=check_program;}
  end

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
     PCons(EmitJasm.pass,
     PCons(ChooseVariableIndex.pass,
     PNil))))
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
