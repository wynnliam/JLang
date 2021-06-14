open Util
open Primop

type label = string

type arg =
  | Imm of Int32.t
  | Var of string (* TODO: Delete this *)

type stack_frame =
  | Full of int
  | Same  (* Same stack frame as ones that jumped to it *)
  | Append of int (* Same stack frame as ones that jumped to it with n additional locals *)
  | Stack1 (* Same stack frame as ones that jumped to it with 1 value (an int in our language) on the stack *)

type instr = 
    (* Pushes a long integer constant onto the stack *)
    (* NOTE: Should be called "Lconst" instead of Push,
       but our language only has one type - integers *)
  | Push of arg
    (* Loads local onto the stack *)
  | Load of arg
    (* Pops value on the stack and stores it *)
  | Store of arg
    (* Pops 2 ints off the stack, adds them, then push onto the stack *)
  | Add
    (* Pops an int off the stack, negates it, then pushes it back onto
       the stack *)
  | Neg
    (* Pop two ints off the stack, do the comparison. If true, go to first label. Otherwise go to second label *)
  | Cmp of comparison * label * label
    (* Pops an operand off the stack *)
  | Pop
    (* Invokes a static method *)
  | InvokeStatic of string
    (* Invokes a virtual method *)
  | Virtual of string
  | Label of label * stack_frame
    (* Unconditional jump to label *)
  | Goto of label
    (* End of a program *)
  | Return

type 'pinfo program = Program of 'pinfo * label * instr list

let readn = "read_int.read:\"()I\""
let writn = "read_int.write:\"(I)V\""

let string_of_stack_frame sf =
  match sf with
  | Same -> "stack_frame_type same"
  | Full n ->
      let cnt = take 0 (n - 1) in
      let cnt' = List.map (fun i -> if i = (n - 1) then "int" else "int, ") cnt in
      List.fold_left (fun acc next -> acc ^ next) "stack_frame_type full;\nlocals_map class \"[Ljava/lang/String;\", " cnt'
  | Append n ->
      let cnt = take 0 (n - 1) in
      let cnt' = List.map (fun i -> if i = (n - 1) then "int" else "int, ") cnt in
      List.fold_left (fun acc next -> acc ^ next) "stack_frame_type append;\nlocals_map " cnt'
  | Stack1 -> "stack_frame_type stack1;\nstack_map int"

let inststr_cmp cmp =
  match cmp with
  | LT -> "lt"
  | LE -> "le"
  | EQ -> "eq"
  | NE -> "ne"
  | GE -> "ge"
  | GT -> "gt"

let print_instruction oc intruction =
  match intruction with
  | Push (Imm i) -> Printf.fprintf oc "ldc int %d;\n" (Int32.to_int i)
  | Push (Var v) -> Printf.fprintf oc "iload %s;\n" v
  | Load (Imm i) -> Printf.fprintf oc "iload %d;\n" (Int32.to_int i)
  | Load (Var v) -> Printf.fprintf oc "iload %s;\n" v
  | Store (Imm i) -> Printf.fprintf oc "istore %l;\n" (Int32.to_int i)
  | Store (Var v) -> Printf.fprintf oc "istore %s;\n" v
  | Add -> Printf.fprintf oc "iadd;\n"
  | Neg -> Printf.fprintf oc "ineg;\n"
  | Cmp (cmp, l1, l2) -> Printf.fprintf oc "if_icmp%s %s;\ngoto %s;\n" (inststr_cmp cmp) l1 l2
  | Pop -> Printf.fprintf oc "pop;\n"
  | InvokeStatic f -> Printf.fprintf oc "invokestatic Method %s;\n" f
  | Virtual v -> Printf.fprintf oc "invokevirtual Method %s;\n" v
  | Label (lbl, sf) -> Printf.fprintf oc "%s:\n%s;\n" lbl (string_of_stack_frame sf)
  | Goto lbl -> Printf.fprintf oc "goto %s;\n" lbl
  | Return -> Printf.fprintf oc "return;\n"

let rec print_instrs oc instrs =
  match instrs with
  | [] -> ()
  | i :: is -> print_instruction oc i; print_instrs oc is

let print_program oc (Program(pinfo, lbl, instrs)) =
  Printf.fprintf oc "%s:\n" lbl;
  print_instrs oc instrs

let print_class_def oc cname =
  Printf.fprintf oc "super class %s\n" cname;
  Printf.fprintf oc "\tversion 59:0\n";
  Printf.fprintf oc "{\n"

let print_class_close oc = Printf.fprintf oc "}"

let emit (bname:string) (Program(pinfo, lbl, instrs)) =
  let oc = open_out (bname ^ ".jasm") in
  print_class_def oc bname;
  print_instrs oc instrs;
  print_class_close oc;
  close_out oc
