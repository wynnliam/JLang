open Util

module Uniquify =
  struct
    open JLoop

    let rec do_exp env = function
	    | Var x -> Var (Env.find x env)
      | Int n -> Int n
      | Let (x,e1,e2) ->
	        let x' = gensym x in
	        Let(x', do_exp env e1, do_exp (Env.add x x' env) e2)
      | Prim (primop,args) -> Prim(primop, List.map (do_exp env) args)
      | Assign(x, e) -> Assign(Env.find x env, do_exp env e)
      | Seq es -> Seq (List.map (do_exp env) es)
      | If (cnd, thn, els) -> If(do_exp env cnd, do_exp env thn, do_exp env els)
      | While(cnd, bdy) -> While(do_exp env cnd, do_exp env bdy)
	    
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
      | Assign(x, e) ->
          if not (Env.mem x (!envr))then
          (Printf.eprintf "Variable %s undeclared!\n" x;
           failwith "Uniquify check failed");
          check_exp e
      | Seq es -> List.iter check_exp es
      | If (cnd, thn, els) -> check_exp cnd; check_exp thn; check_exp els
      | While (cnd, bdy) -> check_exp cnd; check_exp bdy
      | _ -> ()

    let check_program (Program(_,e) as p) =
      envr := Env.empty;
      check_exp e;
      JLoop.check_program false p

    let pass : (unit JLoop.program, unit JLoop.program, unit JLoop.program) pass =
      {name="uniquify";
       transformer=do_program;
       printer=print_program;
       checker=check_program;}
  end (* Uniquify *) 

module EmitJasm =
  struct
    open JasmIf
    let do_op op =
      match op with
      | Primop.Add -> [Add]
      | Primop.Neg -> [Neg]
      | Primop.Read -> [InvokeStatic readn]
      | Primop.Print -> [InvokeStatic writn; Push (Imm 0l)]
      | _ -> assert false (* Compare handled seperately *)

    let env = ref Env.empty
    let next_var_index = ref 1l

    let var_exists (v : string) (env : unit Env.t) =
      match (Env.find_opt v env) with
      | Some i -> ()
      | None -> failwith "Variable not found!"

    let add_var (v : string) =
      match (Env.find_opt v (!env)) with
      | Some i -> failwith "Cannot add variable that already exists!"
      | None ->
          let t = !next_var_index in
          env := Env.add v t (!env);
          next_var_index := Int32.add !next_var_index 1l;
          t

    let find_var (v : string) =
      match (Env.find_opt v (!env)) with
      | None -> failwith ("Undeclare variable" ^ v)
      | Some i -> i

    let rec do_exp exp =
      match exp with
      | JLoop.Int i -> [Push (Imm i)]
      | JLoop.Var v -> [Load (Imm (find_var v))]
      | JLoop.Prim(Compare cmp, [exp1; exp2]) ->
          let lblthn = genlbl "Lcmpt" in
          let lblels = genlbl "Lcmpf" in
          let lbldon = genlbl "Lcmpd" in
          let thnblk = [Label (lblthn, Same); Push (Imm 1l); Goto lbldon] in
          let elsblk = [Label (lblels, Same); Push (Imm 0l); Goto lbldon] in
          let donblk = [Label (lbldon, Stack1)] in
          let exp1' = do_exp exp1 in
          let exp2' = do_exp exp2 in
          exp1' @ exp2' @ [Cmp (cmp, lblthn, lblels)] @ thnblk @ elsblk @ donblk
      | JLoop.Prim(op, args) ->
          let f = fun acc e -> (do_exp e) @ acc in
          let args' = List.fold_left f [] args in
          let op' = do_op op in
          args' @ op'
      | JLoop.Let(v, e1, e2) ->
          let e1' = do_exp e1 in
          let v' = [Store(Imm(add_var v))] in
          let e2' = do_exp e2 in
          e1' @ v' @ e2'
      | JLoop.Assign(v, e) ->
          let e' = do_exp e in
          let i' = find_var v in
          e' @ [Store(Imm i'); Load(Imm i')]
      | JLoop.Seq es ->
          let k = ref 0 in
          let f = fun acc e ->
            let pop = if !k = ((List.length es) - 1) then [] else [Pop] in
            k := !k + 1;
            acc @ (do_exp e) @ pop in
          List.fold_left f [] es
      | JLoop.If (cnd,thn,els) ->
          let lblthn = genlbl "Lthn" in
          let lblels = genlbl "Lels" in
          let lbldon = genlbl "Ldon" in
          let cnd' = do_exp cnd @ [Push (Imm 0l); Cmp (NE, lblthn, lblels)] in
          let thn' = [Label (lblthn, Same)] @ (do_exp thn) @ [Goto lbldon] in
          let els' = [Label (lblels, Same)] @ (do_exp els) @ [Goto lbldon] in
          let don = [Label(lbldon, Stack1)] in
          cnd' @ thn' @ els' @ don
      | JLoop.While (cnd, bdy) ->
          let lblcnd = genlbl "Lcnd" in
          let lblbdy = genlbl "Lbdy" in
          let lbldon = genlbl "Ldon" in
          let cnd' =
            [Label (lblcnd, Same)] @ (do_exp cnd) @ [Push (Imm 0l); Cmp (NE, lblbdy, lbldon)] in
          let bdy' =
            [Label (lblbdy, Same)] @ (do_exp bdy) @ [Pop; Goto lblcnd] in
          let don = [Label (lbldon, Same); Push (Imm 0l)] in
          cnd' @ bdy' @ don

    let emit_jasm expr = do_exp expr

    let compute_intro env =
      let cnt = Env.cardinal env in
      match cnt with
      | 0 -> [Label ("Lmain", Same)]
      | n ->
          let f = fun v p acc -> acc @ [Push (Imm 0l); Store (Imm p)] in
          (Env.fold f env []) @ [Label ("Lmain", Full n)]

    let do_program (JLoop.Program(pinfo, expr)) =
      fresh := 0;
      env := Env.empty;
      let expr' = emit_jasm expr in
      Program(!env, "main", (compute_intro !env) @ expr' @ [Return])

    let check_instr (instr : instr) =
      match instr with
      | Push (Var v) -> failwith "Cannot push Vars. Only Imms!"
      | Load (Var v) -> failwith "Cannot load Vars. Must be an index!"
      | Store (Var v) -> failwith "Cannot store Vars. Must be an index!"
      | _ -> instr

    let check_program (Program (pinfo, lbl, instrs)) =
      let instrs' = List.map check_instr instrs in
      Program(!env, lbl, instrs')

    let pass : (unit JLoop.program,
                Int32.t Util.Env.t JasmIf.program,
                Int32.t Util.Env.t JasmIf.program) pass =
      {name="emit jasm";
       transformer=do_program;
       printer=print_program;
       checker=check_program;}
  end

(* This pass is always required: it does static checking on the source and can be used to obtain the
   "correct" result of evaluation. *)
let initial_pass : (unit JLoop.program, unit JLoop.program, unit JLoop.program) pass = 
  {name="source checking";
   transformer=(fun p -> p);
   printer=JLoop.print_program;	      
   checker=JLoop.check_program true;}

let emit_pass : (Int32.t Util.Env.t JasmIf.program, Int32.t Util.Env.t JasmIf.program, Int32.t Util.Env.t JasmIf.program) pass =  
  {name="emit";
   transformer=(fun p -> p);
   printer=(fun oc p -> ());
   checker=(fun p -> emit JasmIf.emit p; p)}

(* Define sequence of passes for this Chapter.
   Adjust this as you implement more passes.
 *)
let passes = 
     PCons(initial_pass,
     PCons(Uniquify.pass,
     PCons(EmitJasm.pass,
     PCons(emit_pass,
     PNil))))
   
(* Some specializations of Util functions.
   You may wish to alter the initial boolean flags. 
   You may also wish to adjust the Util.debug_level flag *)

let _ = Util.debug_level := 0

(* Suitable for top-level use. *)
let test_string = Util.test_string true false JLoop.parse_from_string passes 

(* Suitable for bulk testing with comparison between first and last pass results. *)
let test_files = Util.test_files false true JLoop.parse_from_channel passes 

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
