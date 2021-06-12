open Util

(*
  sipush int -- pushes an int
  istore_n -- stores int on stack at local variable n
  iload_n -- loads as int variable at local n
  iadd -- pops the two values on the stack, adds them, places result
          back on stack
  ineg -- negates the current item on the stack

  made up Liam Instructions
  invokestatic Method toString.... invokes toString method
  invokevirtual Method println.... invokes printLn method
*)

type arg =
  | Imm of Int32.t
  | Var of string

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

type label = string
type 'pinfo program = Program of 'pinfo * label * instr list

let readn = "read_int.read:\"()I\";"
let writn = "read_int.write:\"(I)V\";"

let print_instruction oc intruction =
  match intruction with
  | Push (Imm i) -> Printf.fprintf oc "bipush %d\n" (Int32.to_int i)
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

let rec print_instrs oc instrs =
  match instrs with
  | [] -> ()
  | i :: is -> print_instruction oc i; print_instrs oc is

let print_program oc (Program(pinfo, lbl, instrs)) =
  Printf.fprintf oc "%s:\n" lbl;
  print_instrs oc instrs
