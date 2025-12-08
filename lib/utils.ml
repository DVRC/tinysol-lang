open Ast
open Cli_ast

(******************************************************************************)
(*                Conversion between values and expressions                   *)
(******************************************************************************)

let is_val = function
  | BoolConst _ 
  | IntConst _
  | AddrConst _ -> true
  | _ -> false

let exprval_of_expr = function
  | BoolConst b -> (Bool b)
  | IntConst n  -> (Int n)
  | AddrConst s -> (Addr s)
  | _ -> failwith ("expression is not a value")

let int_of_expr e = match e with 
  | IntConst n  -> n
  | _ -> failwith "IntConst was expected"

let bool_of_expr e = match e with 
  | BoolConst b -> b
  | _  -> failwith "True or False was expected"

let addr_of_expr e = match e with 
  | AddrConst a -> a
  | _ -> failwith "AddrConst was expected"

let expr_of_exprval = function
  | Bool b -> BoolConst b
  | Int n -> IntConst n
  | Addr b -> AddrConst b
  | Map _ -> failwith "step_expr: wrong type checking of map?"

let addr_of_exprval v = match v with 
  | Addr a -> a
  | Bool _ -> failwith "value has type Bool but an Addr was expected"
  | Int _ -> failwith "value has type Int but an Addr was expected"
  | Map _ -> failwith "value has type Map but an Addr was expected"


(******************************************************************************)
(*                                   List utilities                           *)
(******************************************************************************)

let rec last = function
    [] -> failwith "last on empty list"
  | [st] -> st
  | _::l -> last l

let find_index f l =  
  let rec find_index_helper (b,i) f = function 
    [] -> (b,i)
  | x::l -> if b then (b,i)
            else if f x then (true,i) 
            else find_index_helper (b,i+1) f l 
  in 
    let (b,i) = find_index_helper (false,0) f l in
    if b then Some i else None

(******************************************************************************)
(*                                   File utilities                           *)
(******************************************************************************)

(* read file, and output it to a string *)

let read_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch; s

(* read line from standard input, and output it to a string *)

let read_line () =
  try Some(read_line())
  with End_of_file -> None
;;

let read_lines filename =
  let chan = open_in filename in
  let rec loop acc =
    match input_line chan with
    | line -> loop (line :: acc)
    | exception End_of_file ->
        close_in chan;
        List.rev acc
  in
  loop []

(******************************************************************************)
(*                                   Parsing utilities                        *)
(******************************************************************************)

let parse_expr (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.expr_eof Lexer.read_token lexbuf in
  ast

let parse_cmd (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.cmd_eof Lexer.read_token lexbuf in
  ast

let parse_contract (s : string) : contract =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.contract Lexer.read_token lexbuf in
  ast

let parse_transaction (s : string) : transaction =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.transaction Lexer.read_token lexbuf in
  ast

let parse_cli_cmd (s : string) : cli_cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.cli_cmd Lexer.read_token lexbuf in
  ast

(******************************************************************************)
(*                 Transform inline declarations into blocks                  *)
(******************************************************************************)

let rec gather_decls = function
  | Decl d -> [d]
  | Seq(c1,c2) -> gather_decls c1 @ gather_decls c2
  | _ -> []

let rec purge_decls = function
  | Decl _ -> Skip
  | Seq(Decl _,c2) -> purge_decls c2
  | Seq(c1,Decl _) -> purge_decls c1
  | Seq(c1,c2) -> Seq(purge_decls c1, purge_decls c2)
  | Block(vdl,c) -> Block(vdl @ gather_decls c, purge_decls c)
  | _ as c -> c 

let rec blockify_cmd c = 
  let vdl = gather_decls c in
  let c' = purge_decls c in
  if vdl=[] then blockify_subterms c'
  else Block(vdl, blockify_subterms c')

and blockify_subterms = function
  | Block(vdl,c) -> Block(vdl, blockify_subterms c) 
  | Seq(c1,c2) -> Seq(blockify_subterms c1, blockify_subterms c2) 
  | If(e,c1,c2) -> If(e, blockify_cmd c1, blockify_cmd c2)
  | _ as c -> c

let blockify_fun = function
  | Constr (al,c,p) -> Constr (al,blockify_cmd c,p)
  | Proc (f,al,c,v,p,r) -> Proc(f,al,blockify_cmd c,v,p,r)

let blockify_contract (Contract(c,el,vdl,fdl)) =
  Contract(c,el,vdl,List.map blockify_fun fdl)
