open Util

type label = string

type arg =
  | Imm of Int32.t
  | Var of string (* TODO: Delete this *)

type stack_frame =
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
    (* Pops an operand off the stack *)
  | Pop
    (* Invokes a static method *)
  | InvokeStatic of string
    (* Invokes a virtual method *)
  | Virtual of string
  | Label of label * stack_frame

type 'pinfo program = Program of 'pinfo * label * instr list

let readn = "read_int.read:\"()I\";"
let writn = "read_int.write:\"(I)V\";"

let string_of_stack_frame sf =
  match sf with
  | Same -> "stack_frame_type: same;"
  | Append n ->
      let cnt = take 0 (n - 1) in
      let cnt' = List.map (fun i -> if i = (n - 1) then "int;" else "int, ") cnt in
      List.fold_left (fun acc next -> acc ^ next) "stack_frame_type: append;\nlocals_map " cnt'
  | Stack1 -> "stack_frame_type: stack1;\nstack_map int"

let print_instruction oc intruction =
  match intruction with
  | Push (Imm i) -> Printf.fprintf oc "ldc int %d\n" (Int32.to_int i)
  | Push (Var v) -> Printf.fprintf oc "iload_%s\n" v
  | Load (Imm i) -> Printf.fprintf oc "iload_%d\n" (Int32.to_int i)
  | Load (Var v) -> Printf.fprintf oc "iload_%s\n" v
  | Store (Imm i) -> Printf.fprintf oc "istore_%l\n" (Int32.to_int i)
  | Store (Var v) -> Printf.fprintf oc "istore_%s\n" v
  | Add -> Printf.fprintf oc "iadd\n"
  | Neg -> Printf.fprintf oc "ineg\n"
  | Pop -> Printf.fprintf oc "pop\n"
  | InvokeStatic f -> Printf.fprintf oc "invokestatic Method %s\n" f
  | Virtual v -> Printf.fprintf oc "invokevirtual Method %s\n" v
  | Label (lbl, sf) -> Printf.fprintf oc "%s:\n%s" lbl (string_of_stack_frame sf)

let rec print_instrs oc instrs =
  match instrs with
  | [] -> ()
  | i :: is -> print_instruction oc i; print_instrs oc is

let print_program oc (Program(pinfo, lbl, instrs)) =
  Printf.fprintf oc "%s:\n" lbl;
  print_instrs oc instrs
