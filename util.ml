(* UTILITIES *)

type value = int64

(* TRY-FINALLY *)

let try_finalize f x finally y =
  let res = try f x with exn -> finally y; raise exn in
  finally y;
  res

let take (min : int) (max : int) : int list =
  let rec f (i : int) =
    if i = max then [max] else i :: (f (i + 1))
  in f min

(* USER-FACING EXCEPTIONS *)

exception ParseError
exception UserRuntimeError
exception UserStaticError 

(* ENVIRONMENTS *)

module StringKey = struct type t = string let compare = String.compare end
module Env = Map.Make(StringKey)

let print_env print_value oc env = 
  Printf.fprintf oc "environment:\n\t";
  Env.iter (fun k v -> Printf.fprintf oc "%s:%a " k print_value v) env;
  Printf.fprintf oc "\n"
    
(* GENSYM *)

let fresh = ref 0
    
let gensym s =
  incr fresh;
  s ^ "." ^ (string_of_int (!fresh)) 

(* READ *)

let read_chan = ref stdin  (* change this to read from an arbitrary channel *)

let read_int () =
  match Int64.of_string_opt(input_line (!read_chan))
  with
    Some i -> i
  | None -> (prerr_endline "invalid integer entered";
	     raise UserRuntimeError)

let with_input_file (fname:string) (action: unit -> 'a) : 'a =
  let ic = open_in fname in
  read_chan := ic;
  try_finalize action () (fun ic -> close_in ic; read_chan := stdin) ic


(* OSTYPE *)

type ostype = Unix | MacOS
let get_ostype () =
  let uname() =                                                      
    let inc = Unix.open_process_in "uname -s" in  
    let name = input_line inc in                  
    close_in inc;
    name in
  if uname() = "Darwin" then MacOS else Unix

(* EXTERNAL ASSEMBLY AND EXECUTION *)

let assemble_and_link (bname:string) =
  let com = Printf.sprintf "gcc -o %s %s.s runtime.c\n" bname bname in
  assert (Sys.command com = 0)
  
let run (bname:string) (infile_opt:string option) : value (* actually 8 unsigned bits *)  =
  let com = 
    match infile_opt with
    | None -> Printf.sprintf "./%s" bname 
    | Some infile -> Printf.sprintf "./%s < %s" bname infile in
  Int64.of_int (Sys.command com)
    
(* PASSES  *)

type ('a,'b,'c) pass =
    { name : string;
      transformer: 'a -> 'b;
      printer: out_channel -> 'b -> unit;
      checker: 'b -> 'c;
    }

type ('a,'b) passlist =  (* uses as GADT (generalized algebraic data type) to existentially quantify 'b *)
    PNil : ('a,'b) passlist
  | PCons : ('a,'b,'c) pass * ('c,'d) passlist -> ('a,'d) passlist

(* EXECUTION ENVIRONMENT *)

let debug_level = ref 0   (* 0 = almost nothing; 1 = report steps; 2 = dump intermediate forms; 3 = show interpreter steps *)

let infile_opt = ref None  (* Source for reads *)

let base_name = ref ""  (* Name of source file, with directory but without extension *)

(* EVALUATORS *)

let interpret (interpreter: 'a -> value) (program: 'a) : value =
  match !infile_opt with
  | None -> interpreter program (* this will use stdin for input, which will not work properly for repeated executions that read! *)
  | Some infile -> with_input_file infile (fun () -> interpreter program)

let emit (emitter: string -> 'a -> unit) (program: 'a) : unit =
  if !debug_level > 1 then Printf.eprintf "Emitting...%!";
  emitter (!base_name) program

let emit_and_run (emitter: string -> 'a -> unit) (program: 'a) : value = 
  if !debug_level > 1 then Printf.eprintf "Emitting...%!";
  emitter (!base_name) program;
  if !debug_level > 1 then Printf.eprintf "Assembling and linking...%!";
  assemble_and_link (!base_name);
  if !debug_level > 1 then Printf.eprintf "Running...%!";
  run (!base_name) (!infile_opt)

(* Perform a pass *)
let do_pass (p: ('a,'b,'c) pass) (prog:'a) : 'c =
  if !debug_level > 0 then Printf.eprintf "PASS: %s\n%!" p.name;
  let prog = p.transformer prog in
  if !debug_level > 1 then
    (Printf.eprintf ("Raw pass result:\n"); 
     p.printer stderr prog;
     flush stderr);
  if !debug_level > 1 then Printf.eprintf ("Checking...\n%!");
  let prog = p.checker prog in
  prog		       

(* Perform a list of passes.
   Flags:
    - eval: evaluate the last pass and displays the result
    - compare: if there are at least two passes, evaluate the first and last passes and
      flag differernces between initial and final results  *)
let do_passes (eval: bool) (compare: bool) (pl: ('a,'b) passlist) (prog:'a) : unit =
  match pl with
  | PNil -> ()
  | PCons (p,pl) ->
      let prog = do_pass p prog in
      let rec do_passlist : 'a 'b . ('a,'b) passlist -> 'a -> unit = fun pl prog -> 
	match pl with
	| PNil -> ()
  | PCons(p,pl) ->
	    do_passlist pl (do_pass p prog) in
      do_passlist pl prog

(* TOP-LEVEL DRIVER FUNCTIONS *)

(* Process a single file
   Arguments:
    eval: always print result of evaluating last pass
    compare : warn of discrepencies between first and last pass evaluations
    parse_from_chan : parse source program from specified in_channel
    passes : list of passes to perform
    fname: full name of file to process
   Automatically looks for a matching ".in" file and uses it when available.
   Output results to stderr.  
   User-facing errors output message to stderr and continue. *)
let test_file
    (eval : bool)
    (compare : bool)
    (parse_from_chan : in_channel -> 'a)
    (passes : ('a,'b) passlist)
    (fname: string) : 'c = 
  try 
    let ic = open_in fname in 
    let prog = try_finalize parse_from_chan ic close_in ic in
    base_name := Filename.remove_extension fname;
    let inname = !base_name ^ ".in" in
    infile_opt := if Sys.file_exists inname then Some inname else None;
    do_passes eval compare passes prog 
  with
    (ParseError | UserStaticError | UserRuntimeError) ->
      prerr_endline "Failed with user error"
	
(* Process all files from specified list of fnames. Args as above. *)
let test_files eval compare parse_from_chan passes fnames = 
  List.iter
    (fun fname ->
      Printf.eprintf "Testing: %s\n%!" fname; 
      test_file eval compare parse_from_chan passes fname)
    fnames

(* Process a single string. Suitable for top-level testing of a single sexp. Args as above.
   Puts generated assembly code and executable into tests/adhoc[.s]
   Note that any (read)'s will be performed from stdin, which doesn't work well if you set
   the compare flag to true! *)
let test_string (eval:bool) (compare:bool) parse_from_string passes s =
  try 
    let prog = parse_from_string s in
    base_name := "tests/adhoc";
    infile_opt := None;
    do_passes eval compare passes prog
  with
    (ParseError | UserStaticError | UserRuntimeError) ->
      prerr_endline "Failed with user error"
    
