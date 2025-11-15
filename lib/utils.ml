open Ast
open Types

let rec union l1 l2 = match l1 with
    [] -> l2
  | x::l1' -> (if List.mem x l2 then [] else [x]) @ union l1' l2

let rec vars_of_expr = function
    True
  | False
  | IntConst _ -> []
  | AddrConst _ -> []               
  | Var x -> [x]
  | Not e -> vars_of_expr e
  | And(e1,e2) 
  | Or(e1,e2) 
  | Add(e1,e2)
  | Sub(e1,e2)
  | Mul(e1,e2) 
  | Eq(e1,e2) 
  | Neq(e1,e2) 
  | Leq(e1,e2) 
  | Le(e1,e2)
  | Geq(e1,e2) 
  | Ge(e1,e2) -> union (vars_of_expr e1) (vars_of_expr e2)                    

and vars_of_cmd = function
    Skip -> []
  | Assign(x,e) -> union [x] (vars_of_expr e)
  | Seq(c1,c2) -> union (vars_of_cmd c1) (vars_of_cmd c2)
  | If(e,c1,c2) -> union (vars_of_expr e) (union (vars_of_cmd c1) (vars_of_cmd c2))
  | Send(x,e,y) -> union [x] (union [y] (vars_of_expr e))
  | Req(e) -> vars_of_expr e                    
  | Call(f,e) -> union [f] (vars_of_expr e)
  | CallExec(c) -> vars_of_cmd c
  | Block(_,c) -> vars_of_cmd c
  | ExecBlock(c) -> vars_of_cmd c

let vars_of_contract (Contract(_,vdl,_)) : ide list = 
  List.fold_left (fun acc vd -> match vd with 
    IntVar x | BoolVar x | AddrVar x -> x::acc ) [] vdl 


let string_of_exprval = function
    Bool b -> string_of_bool b
  | Int n  -> string_of_int n
  | Addr s -> s

let string_of_arg = function
  | IntArg(x) -> x
  | RcvArg(x,t) -> x ^ "?" ^ t

let string_of_modifier = function
  | Public -> "public"
  | Private -> "private"

let string_of_args = List.fold_left (fun s a -> s ^ (if s<>"" then "," else "") ^ (string_of_arg a)) ""

let rec string_of_expr = function
    True -> "true"
  | False -> "false"
  | Var x -> x
  | IntConst n -> string_of_int n
  | AddrConst a -> "\"" ^ a ^ "\""
  | Not e -> "!" ^ string_of_expr e
  | And(e1,e2) -> string_of_expr e1 ^ " && " ^ string_of_expr e2
  | Or(e1,e2) -> string_of_expr e1 ^ " || " ^ string_of_expr e2
  | Add(e1,e2) -> string_of_expr e1 ^ "+" ^ string_of_expr e2
  | Sub(e1,e2) -> string_of_expr e1 ^ "-" ^ string_of_expr e2
  | Mul(e1,e2) -> string_of_expr e1 ^ "*" ^ string_of_expr e2
  | Eq(e1,e2) -> string_of_expr e1 ^ "==" ^ string_of_expr e2
  | Neq(e1,e2) -> string_of_expr e1 ^ "!=" ^ string_of_expr e2
  | Leq(e1,e2) -> string_of_expr e1 ^ "<=" ^ string_of_expr e2
  | Le(e1,e2) -> string_of_expr e1 ^ "<" ^ string_of_expr e2                    
  | Geq(e1,e2) -> string_of_expr e1 ^ ">=" ^ string_of_expr e2
  | Ge(e1,e2) -> string_of_expr e1 ^ ">" ^ string_of_expr e2                    

and string_of_cmd = function
    Skip -> "skip;"
  | Assign(x,e) -> x ^ "=" ^ string_of_expr e ^ ";"
  | Seq(c1,c2) -> string_of_cmd c1 ^ " " ^ string_of_cmd c2
  | If(e,c1,c2) -> "if (" ^ string_of_expr e ^ ") {" ^ string_of_cmd c1 ^ "} else {" ^ string_of_cmd c2 ^ "}"
  | Send(x,e,t) -> x ^ ".transfer(" ^ (string_of_expr e) ^ ":" ^ t ^ ");"
  | Req(e) -> "require " ^ string_of_expr e ^ ";"
  | Call(f,e) -> f ^ "(" ^ string_of_expr e ^ ")"
  | CallExec(c) -> "exec{" ^ string_of_cmd c ^ "}"
  | Block(vdl,c) -> "{" 
    ^ List.fold_left (fun s d -> s ^ (if s<>"" then "; " else "") ^ string_of_var_decl d) "" vdl ^ ";" 
    ^ string_of_cmd c 
    ^ "}"
  | ExecBlock(c) -> "{" 
    ^ string_of_cmd c 
    ^ "}"
and string_of_var_decl = function
  | IntVar(x) -> "int " ^ x
  | BoolVar(x) -> "bool " ^ x
  | AddrVar(x) -> "address " ^ x

and string_of_fun_decl = function Proc(f,a,c,m) -> 
    if f="constructor" then
      "constructor " ^ f ^ "(" ^ (string_of_args a) ^ ") {" ^ string_of_cmd c ^ "}\n"
    else
    "function " ^ f ^ "(" ^ (string_of_args a) ^ ") " ^
    string_of_modifier m ^ " " ^ 
    "{" ^ string_of_cmd c ^ "}\n"

let string_of_var_decls = List.fold_left (fun s d -> s ^ (if s<>"" then ";\n  " else "  ") ^ string_of_var_decl d) ""

let string_of_fun_decls = List.fold_left (fun s d -> s ^ (if s<>"" then "  " else " ") ^ string_of_fun_decl d) ""

let string_of_contract (Contract(c,vdl,fdl)) = 
  "contract " ^ c ^ 
  " {\n" ^ 
  (let s = string_of_var_decls vdl in if s="" then "" else s ^ ";\n ") ^ 
  (string_of_fun_decls fdl) ^ 
  " }"

(*
let string_of_env1 s x = match topenv s x with
  | IVar v -> string_of_exprval v ^ "/" ^ x
  | IProc(a,c) -> "fun(" ^ string_of_args a ^ "){" ^ string_of_cmd c ^ "}/" ^ x
*)

let string_of_env e vl = 
  let rec helper e vl = match vl with 
  | [] -> ""
  | x::vl' -> try (x ^ "->" ^
    match e x with 
      | Bool b -> string_of_bool b
      | Int  n -> string_of_int n
      | Addr a -> a
    )
    with _ -> helper e vl' 
  in "{" ^ helper e vl ^ "}"

let string_of_envstack el vl = 
  let rec helper el vl = match el with
    [] -> ""
  | [e] -> (string_of_env e vl)
  | e::el' -> (string_of_env e vl) ^ ";" ^ (helper el' vl)
in "[" ^ (helper el vl) ^ "]" 

let rec range a b = if b<a then [] else a::(range (a+1) b);;

let string_of_storage (stg : ide -> exprval) (xl : ide list) =
  List.fold_left (fun acc x -> acc ^ x ^ "=" ^ (string_of_exprval (stg x)) ^ "; ") "" xl

let string_of_account_state accst =
  "{ " ^
  "balance=" ^ string_of_int accst.balance ^ "; " ^
  (match accst.code with 
  | None -> ""
  | Some src -> string_of_storage accst.storage (vars_of_contract src)) ^
  "}"

let string_of_accounts (st : sysstate) =
  "[" ^ 
  (List.fold_left (fun acc a -> acc ^ a ^ " -> " ^ (string_of_account_state (st.accounts a)) ^ " ") "" st.active) ^ 
  "]"

(* TODO: add storage variables *)

let string_of_sysstate (st : sysstate) (evl : ide list) =
  "accounts: " ^ 
  string_of_accounts st ^ 
  "\n" ^
  "envstack: " ^
  string_of_envstack st.stackenv evl

let string_of_execstate evl = function
  | St st -> string_of_sysstate st evl
  | Cmd (c,st,a) -> 
      "cmd: " ^ (string_of_cmd c) ^ "\n" ^ 
      (string_of_sysstate st evl) ^ "," ^ a 

let string_of_trace stl = match stl with
  [] -> ""
| St _::_ -> ""
| Cmd (c,_,_)::_ -> let evl = vars_of_cmd c in  
  let rec helper stl = (match stl with
    [] -> ""
  | [st] -> (string_of_execstate evl st)
  | st::l -> (string_of_execstate evl st) ^ "\n--->\n" ^ helper l)
in helper stl

let rec last = function
    [] -> failwith "last on empty list"
  | [x] -> x
  | _::l -> last l
