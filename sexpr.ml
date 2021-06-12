(* Representing, printing and parsing sexpressions *)

type sexp =
  | SList of sexp list 
  | SNum of int32
  | SSym of string
  | SString of string 

(* Parsing of expressions *)

module Parse : 
  sig 
    val parse : char Stream.t -> sexp
    exception ParseError
  end =
struct
  type lineno = int

  exception ParseError
  exception Parse_failed of string * lineno

  type token = 
  | Num of int32
  | Sym of string
  | String of string
  | Lp
  | Rp
  | Eof

  let is_digit (c:char) : bool = String.contains "0123456789" c
  let is_whitespace (c:char) : bool = String.contains " \012\t\r\n" c
  let is_string_char (c: char) : bool = not (String.contains "\"\012\r\n" c)
  (* excludes digits, whitespace, {},(), and ` *)
  let is_sym_char (c:char) : bool =
    String.contains "!#$%&'*+,-./:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_abcdefghijklmnopqrstuvwxyz|~" c

  let tokenize (cs: char Stream.t) : (token*lineno) Stream.t =
    let lineno = ref 1 in
    let mk_num (s:string) : token =
      match Int32.of_string_opt s with
	Some i -> Num i
      | None -> raise (Parse_failed ("Invalid number: " ^ s, !lineno)) in
    let mk_sym (s:string) : token = Sym s in 
    let mk_string (s:string) : token = String s in 
    let comment_level = ref 0 in 
    let buff = Buffer.create 16 in 
    let rec collect (test: char -> bool) (build : string -> token) =
      match Stream.peek cs with
      | Some c when test c -> 
	  (Stream.junk cs;
	   Buffer.add_char buff c;
           collect test build)
      |	_ -> Some (build (Buffer.contents buff),!lineno) in
    let next_is_digit () : bool =
      match Stream.peek cs with
      | Some c when is_digit c -> true
      | _ -> false in
    let rec tok count =  (* argument is ignored *)
      match Stream.peek cs with
      |	Some c -> 
	Stream.junk cs;
        begin match c with
	| '\n' -> (incr lineno; tok count)
	| '{' -> (incr comment_level; tok count)
	| '}' -> if !comment_level > 0 then 
                   (decr comment_level; tok count)
                 else 
                   raise (Parse_failed("Unmatched close comment", !lineno))
	| _ when !comment_level > 0 -> tok count
	| '\"' -> (Buffer.clear buff;
	           let s = collect is_string_char mk_string in
		   match Stream.peek cs with
 		    | Some '\"' -> Stream.junk cs; s
		    | _ -> raise (Parse_failed("Unterminated string literal",!lineno)))
	| _ when is_whitespace c -> tok count
	| '(' -> Some (Lp,!lineno) 
	| ')' -> Some (Rp,!lineno)
	| _ when is_digit c -> (Buffer.clear buff; Buffer.add_char buff c; 
				collect is_digit mk_num)
        | '-' when next_is_digit () -> (Buffer.clear buff; Buffer.add_char buff c; 
				     collect is_digit mk_num)
	| _ when is_sym_char c -> (Buffer.clear buff; Buffer.add_char buff c;
				    collect (fun c -> is_sym_char c || is_digit c) mk_sym) 
	| _ -> raise (Parse_failed ("Invalid character: " ^ String.make 1 c, !lineno))
	end
      |	None -> 
        if !comment_level = 0 then
	  Some(Eof,!lineno)
        else
          raise (Parse_failed ("Unmatched open comment",!lineno)) in
    Stream.from tok 

  let rec parse_sexp (ts: (token * lineno) Stream.t) : sexp =
    match Stream.next ts with
    | (Lp,_) -> SList(parse_sexp_list ts)
    | (Num n,_) -> SNum n
    | (Sym s,_) -> SSym s
    | (String s,_) -> SString s
    | (_,lineno) -> raise (Parse_failed ("Missing or invalid expression",lineno))
  and parse_sexp_list ts : sexp list =
    match Stream.peek ts with
    | Some (Rp,_) -> (Stream.junk ts; [])
    | _ ->
	let sexp = parse_sexp ts in
	let sexpl = parse_sexp_list ts in
	sexp :: sexpl

  let parse (cs: char Stream.t) : sexp =
    try  
      let ts = tokenize cs in
      let exp = parse_sexp ts in 
      match Stream.next ts with
      | (Eof,_) -> exp
      | (_,lineno) -> raise (Parse_failed ("Extraneous characters at end of input",lineno))
    with Parse_failed(s,lineno) ->
      (Printf.eprintf "Sexpr parse failed: %s at line %d\n" s lineno;
       raise ParseError)

end (* Parse *)


module Print :
    sig
      val print : out_channel -> sexp -> unit
    end = 
  struct
    open Printf
    
    let rec print_sexp indent oc = function
	    | SNum i -> output_string oc (Int32.to_string i)
      | SString s -> fprintf oc "\"%s\"" s
      | SSym s -> output_string oc s
      | SList [] -> output_string oc "()"
      | SList [s] -> fprintf oc "(%a)" (print_sexp indent) s
      | SList ss -> (if indent > 0 then output_string oc "\n";
		     output_string oc (String.make indent ' ' ^ "(");
		     print_list (indent+1) oc ss;
		     output_string oc (")"))
    and print_list indent oc = function
	[] -> ()
      | [e] -> print_sexp indent oc e
      | e::es -> (print_sexp indent oc e; output_string oc " "; print_list indent oc es)

    let print oc sexp =
      print_sexp 0 oc sexp;
      output_string oc "\n"

  end  (* Print *)

exception ParseError = Parse.ParseError
let parse_from_channel ic = Parse.parse (Stream.of_channel ic)
let parse_from_string s = Parse.parse (Stream.of_string s)
let print = Print.print

      
