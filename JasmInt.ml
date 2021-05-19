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
    (* Pushes an integer constant onto the stack *)
  | Push of arg
    (* Loads local onto the stack *)
  | Load of arg
    (* Pops 2 ints off the stack, adds them, then push onto the stack *)
  | Add
    (* Pops an int off the stack, negates it, then pushes it back onto
       the stack *)
  | Neg
    (* Invokes read_int.read() *)
  | Read of arg
    (* Invokes read_int.write() *)
  | Write of arg
