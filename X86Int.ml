open Util

(* ABSTRACT SYNTAX *)

type reg =
    RSP
  | RBP
  | RAX
  | RBX
  | RCX
  | RDX
  | RSI
  | RDI
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15


type label = string

type arg =
    Imm of Int64.t  (* in most cases must actually be an Int32 *)
  | Reg of reg
  | Deref of reg*Int32.t
  | Var of string  (* a pseudo-argument *)

type instr =
    Addq of arg*arg
  | Subq of arg*arg
  | Movq of arg*arg
  | Movabsq of arg*arg
  | Negq of arg
  | Callq of label*int
  | Retq
  | Pushq of arg
  | Popq of arg
  | Jmp of label

type 'binfo block = Block of 'binfo * instr list

type ('pinfo,'binfo) program = Program of 'pinfo * (label * 'binfo block) list 


(* UTILITY FUNCTIONS *)    

(* Generate main function entry and exit boilerplate, suitable for pseudo-X86 (`framesize to be filled in later)  *)
let func_entry_exit : (label * unit block) list = 
   [("main",
     Block((),
	   [Pushq(Reg RBP);
	    Movq(Reg RSP,Reg RBP);
	    Subq(Var "`framesize",Reg RSP);  (* an invented variable (note the back-tick `) *)
	    Jmp ("start")]));
    ("conclusion",
     Block((),
	   [Addq(Var "`framesize",Reg RSP);
	    Popq(Reg RBP);
	    Retq]))] 


(* Check if an Int64 number requires more than 32 bits to represent *)
let needs_64  i = i > Int64.of_int32(Int32.max_int) || i < Int64.of_int32(Int32.min_int)

(* Round x up to nearest multiple of p, provided p is a multiple of 2 *)
let roundup x p = (x + p - 1) land (lnot(p-1))


(* PRINTING *)

(* These functions produce output in AT&T assembler format, suitable for input to an assembler. *)

open Format

let show_lab fmt lab =
  let lab' = 
    match get_ostype() with
   | MacOS -> "_" ^ lab
   | _ -> lab  in
  fprintf fmt "%s" lab'

let show_reg fmt r =
  let rname = 
    match r with 
    | RSP -> "rsp"
    | RBP -> "rbp"
    | RAX -> "rax"
    | RBX -> "rbx"
    | RCX -> "rcx"
    | RDX -> "rdx"
    | RSI -> "rsi"
    | RDI -> "rdi"
    | R8 -> "r8"
    | R9 -> "r9"
    | R10 -> "r10"
    | R11 -> "r11"
    | R12 -> "r12"
    | R13 -> "r13"
    | R14 -> "r14"
    | R15 -> "r15" in
  fprintf fmt "%s" rname 

let show_arg fmt = function
  | Imm i -> fprintf fmt "$%Ld" i
  | Reg r -> fprintf fmt "%%%a" show_reg r
  | Deref(r,i) -> fprintf fmt "%ld(%%%a)" i show_reg r
  | Var s -> fprintf fmt "%s" s

let show_instr fmt = function
  | Addq(a1,a2) -> fprintf fmt "addq\t%a, %a" show_arg a1 show_arg a2
  | Subq(a1,a2) -> fprintf fmt "subq\t%a, %a" show_arg a1 show_arg a2
  | Movq(a1,a2) -> fprintf fmt "movq\t%a, %a" show_arg a1 show_arg a2
  | Movabsq(a1,a2) -> fprintf fmt "movq\t%a, %a" show_arg a1 show_arg a2
  | Negq(a) -> fprintf fmt "negq\t%a" show_arg a
  | Callq(lab,_) -> fprintf fmt "callq\t%a" show_lab lab
  | Retq -> fprintf fmt "retq"
  | Pushq(a) -> fprintf fmt "pushq\t%a" show_arg a
  | Popq(a) -> fprintf fmt "popq\t%a" show_arg a
  | Jmp(lab) -> fprintf fmt "jmp\t%a" show_lab lab

let show_instr_line fmt instr =
  fprintf fmt "\t%a\n" show_instr instr

let show_block fmt (Block(_,instrs)) =
  List.iter (show_instr_line fmt) instrs

let show_labeled_block fmt (lab,block) =
  fprintf fmt "%a:\n" show_lab lab;
  show_block fmt block

let show_program fmt (Program(info,lbs)) =
  fprintf fmt "\t.globl %a\n" show_lab "main"; 
  List.iter (show_labeled_block fmt) lbs

let print_program print_info oc (Program(info,lbs) as p) =
  print_info oc info;
  show_program (formatter_of_out_channel oc)  p

(* Emitting program to a .s file *)
let emit (bname:string) (p: ('a,'b) program) =
  let oc = open_out (bname ^ ".s") in
  print_program (fun _ _ -> ()) oc p;
  close_out oc

(* Convenience functions for error messages, etc. *)
let reg_to_string =
  asprintf "%a" show_reg

let instr_to_string =
  asprintf "%a" show_instr 

(* CHECKING *)

(* Label checking: all label declarations are unique and all jump targets are defined.
   This is suitable for multiple stages of programs (w/wo Var, w/wo legal arg sizes and types).
   N.B. The assembler will check this again later. 
*)
    
module CheckLabels = 
  struct 
    let checkfail s = failwith ("X86Int label check failed: " ^ s)
	
    let check_instr env = function 
      | Jmp l -> if not (Env.mem l env) then checkfail ("jmp to undefined label " ^ l)
      | Callq(l,_) -> if not (Env.mem l env) then checkfail ("callq to undefined label " ^ l)
      | _  -> ()

    let check_block env (Block(_,instrs)) = List.iter (check_instr env) instrs

    let labels_of_lbs lbs env = 
      List.fold_right (fun (lab,_) env ->
	(if Env.mem lab env then checkfail ("multiply defined label " ^ lab);
	 Env.add lab () env))
	lbs env

    let check_program ((Program(_,lbs) as p): ('pinfo,'binfo) program) =
      let external_funcs = ["read_int"] in
      let env = List.fold_right (fun name -> Env.add name ()) external_funcs Env.empty in
      let env = labels_of_lbs lbs env in 
      List.iter (fun (_,b) -> check_block env b) lbs;
      p

  end (* CheckLabels *)

(* Arg checking.
   Try to enforce that all args are legal, including that memory accesses are relative
   to rbp and are within the bounds of the declared frame size.
   This is suitable for Programs where 'pinfo is instantiated to the frame size,
   and there are no remaining Var args or invalid arg combinations.
   N.B. The assembler will check some of this again later, but we can be stricter 
   (e.g. ensuring that Deref operands are within the bounds of the frame size).
*)
    
module CheckArgs =
  struct
    let checkfail s = failwith ("X86Int arg check failed: " ^ s)
	
    let is_reg = function
	Reg _ -> true
      | _ -> false
	    
    let is_mem = function
      | Deref _ -> true
      | _ -> false

    let is_imm = function
      | Imm _ -> true
      | _ -> false
	    
    let is_valid_src framesize = function
      | Imm i when not (needs_64 i) -> true
      | Reg _ -> true
      | Deref(RBP,offset) when ((Int32.neg offset) <= (Int32.of_int framesize))
	                       && (Int32.rem offset 8l = 0l) -> true
      | _ -> false
	    
    let is_valid_dest framesize = function
      | Reg _ -> true
      | Deref(RBP,offset) when (Int32.neg offset) <= (Int32.of_int framesize) -> true
      | _ -> false
	    
    let check_binary_instr framesize instr a1 a2 =
      (if not (is_valid_src framesize a1) then checkfail ("bad source in: " ^ (instr_to_string instr)));
      (if not (is_valid_dest framesize a2) then checkfail ("bad destination in: " ^ (instr_to_string instr)));
      (if is_mem a1 && is_mem a2 then checkfail("source and destination both memory ops in: " ^ (instr_to_string instr)));
      ()

    let check_instr framesize instr =
      match instr with
      | Addq(a1,a2) -> check_binary_instr framesize instr a1 a2
      | Subq(a1,a2) -> check_binary_instr framesize instr a1 a2
      | Movq(a1,a2) -> check_binary_instr framesize instr a1 a2
      | Movabsq(a1,a2) ->
	  (if not (is_imm a1) then checkfail ("bad source in: " ^ (instr_to_string instr)));
	  (if not (is_reg a2) then checkfail ("bad destination in: " ^ (instr_to_string instr)))
      | Negq a ->
	  if not (is_valid_dest framesize a) then checkfail ("bad destination in: " ^ (instr_to_string instr))
      | Pushq a -> 
	  if not (is_valid_src framesize a) then checkfail ("bad source in: " ^ (instr_to_string instr))
      | Popq a -> 
	  if not (is_valid_dest framesize a) then checkfail ("bad destination in: " ^ (instr_to_string instr))
      | _  -> ()

    let check_block framesize (Block(_,instrs)) = List.iter (check_instr framesize) instrs
	
    let check_program ((Program(framesize,lbs) as p): (int,'binfo) program) =
      (if framesize mod 16 <> 0 then checkfail "framesize not multiple of 16");
      List.iter (fun (_,b) -> check_block framesize b) lbs;
      p
  end (* CheckArgs *)


module RegKey = struct type t = reg let compare = compare end
module REnv = Map.Make(RegKey)
let print_renv print_value oc env = 
  REnv.iter (fun k v -> Printf.fprintf oc "%s:%a " (reg_to_string k) print_value v) env
   
module MemKey = struct type t = int64 let compare = compare end
module MEnv = Map.Make(MemKey)    
let print_menv print_value oc env = 
  MEnv.iter (fun k v -> Printf.fprintf oc "%Ld:%a " k print_value v) env
    
let print_venv print_value oc env =
  Env.iter (fun k v -> Printf.fprintf oc "%s:%a " k print_value v) env

type envs = { renv: int64 REnv.t; menv: int64 MEnv.t; venv: int64 Env.t }

let print_envs oc envs =
  let print_value oc = Printf.fprintf oc "%Ld" in
  print_renv print_value oc envs.renv;
  print_menv print_value oc envs.menv;
  print_venv print_value oc envs.venv;
  Printf.fprintf oc "\n%!"

let uninitialized_value = 1010101010101010101L

let interp_arg (envs:envs) arg =
  try 
    match arg with
      Imm i -> i
    | Reg r -> REnv.find r envs.renv
    | Deref (r,offset) -> MEnv.find (Int64.add (REnv.find r envs.renv) (Int64.of_int32 offset)) envs.menv
    | Var s -> Env.find s envs.venv 
  with
    Not_found -> uninitialized_value
    
let update_arg (envs: envs) (a:arg) (v:int64) = 
  match a with
  | Reg r -> { envs with renv = REnv.add r v envs.renv }
  | Deref (r,offset) -> { envs with menv = MEnv.add (Int64.add (interp_arg envs (Reg r)) (Int64.of_int32 offset)) v envs.menv }
  | Var s -> { envs with venv = Env.add s v envs.venv }
  | _ -> assert false

let interp_instr envs instr =
  match instr with
    Addq(a1,a2) ->
      update_arg envs a2 (Int64.add (interp_arg envs a2) (interp_arg envs a1))
  | Subq(a1,a2) ->
      update_arg envs a2 (Int64.sub (interp_arg envs a2) (interp_arg envs a1))
  | Movq(a1,a2) ->
      update_arg envs a2 (interp_arg envs a1)
  | Movabsq(a1,a2) ->
      update_arg envs a2 (interp_arg envs a1)
  | Negq a ->
      update_arg envs a (Int64.neg (interp_arg envs a))
  | Pushq a ->
      let v = interp_arg envs a in
      let envs = update_arg envs (Reg RSP) (Int64.sub (interp_arg envs (Reg RSP)) 8L) in
      update_arg envs (Deref(RSP,0l)) v
  | Popq a ->
      let v = interp_arg envs (Deref(RSP,0l)) in
      let envs = update_arg envs (Reg RSP) (Int64.add (interp_arg envs (Reg RSP)) 8L) in
      update_arg envs a v   (* by my reading of the Intel docs, this is the correct ordering *)
  | Callq("read_int",_) ->
      if Int64.rem (interp_arg envs (Reg RSP)) 16L <> 0L then
	failwith "misaligned RSP at call";
      update_arg envs (Reg RAX) (read_int())
  | _ ->
      assert false

let find_block lbs lab : instr list =
  let Block(_,instrs) = List.assoc lab lbs in
  instrs

let interp_instrs lbs = 
  let rec interp envs = function
      instr::rest -> 
	(if !debug_level > 2 then
	  (prerr_string " ";
           print_envs stderr envs;  (* prints only initialized values! *)
  	   prerr_endline (instr_to_string instr));
	 match instr with
	   Jmp lab -> interp envs (find_block lbs lab)
	 | Retq -> envs
	 | _ -> interp (interp_instr envs instr) rest)
    | _ -> assert false in
  interp
      
let init_envs with_vars =
  { renv = REnv.add RBP 0L (REnv.add RSP (-8L) REnv.empty);
            (* need RSP to be sane to execute a single function body;
	       RBP shouldn't matter, but make it something sane and small to avoid cluttering debug *)
    menv = MEnv.empty;
    venv = if with_vars then
               Env.add "`framesize" 0L Env.empty
                   (* kludge for X86Var: should be ok assuming frame is not being used *)
           else Env.empty
  }

let interp_program (with_vars: bool) (Program(_,lbs)) : value =
  let envs = interp_instrs lbs (init_envs with_vars) (find_block lbs "main") in
  REnv.find RAX envs.renv
