open Ast
open Types

let parse_cmd (s : string) : cmd =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.cmd_test Lexer.read_token lexbuf in
  ast

let parse_contract (s : string) : contract =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.contract Lexer.read_token lexbuf in
  ast
  
exception TypeError of string
exception UnboundVar of string
exception PredOfZero
exception NoRuleApplies


(******************************************************************************)
(*                       Big-step semantics of expressions                    *)
(******************************************************************************)

let is_val = function
    True -> true
  | False -> true
  | IntConst _ -> true
  | AddrConst _ -> true
  | _ -> false

let rec eval_expr (st : sysstate) (a : addr) = function
    True -> Bool true
  | False -> Bool false
  | Var x -> lookup st a x
  | IntConst n -> Int n
  | AddrConst s -> Addr s
  | Not(e) -> (match eval_expr st a e with
        Bool b -> Bool(not b)
      | _ -> raise (TypeError "Not")
    )
  | And(e1,e2) -> (match (eval_expr st a e1,eval_expr st a e2)  with
        (Bool b1,Bool b2) -> Bool(b1 && b2)
      | _ -> raise (TypeError "And")
    )
  | Or(e1,e2) -> (match (eval_expr st a e1,eval_expr st a e2)  with
        (Bool b1,Bool b2) -> Bool(b1 || b2)
      | _ -> raise (TypeError "Or")
    )
  | Add(e1,e2) -> (match (eval_expr st a e1,eval_expr st a e2)  with
        (Int n1,Int n2) -> Int(n1 + n2)
      | _ -> raise (TypeError "Add")
    )    
  | Sub(e1,e2) -> (match (eval_expr st a e1,eval_expr st a e2)  with
        (Int n1,Int n2) when n1>=n2 -> Int(n1 - n2)
      | _ -> raise (TypeError "Sub")
    )
  | Mul(e1,e2) -> (match (eval_expr st a e1,eval_expr st a e2)  with
        (Int n1,Int n2) -> Int(n1 * n2)
      | _ -> raise (TypeError "Add")
    )        
  | Eq(e1,e2) -> (match (eval_expr st a e1,eval_expr st a e2)  with
        (Int n1,Int n2) -> Bool(n1 = n2)
      | _ -> raise (TypeError "Eq")
    )    
  | Neq(e1,e2) -> (match (eval_expr st a e1,eval_expr st a e2)  with
        (Int n1,Int n2) -> Bool(n1 <> n2)
      | _ -> raise (TypeError "Eq")
    )    
  | Leq(e1,e2) -> (match (eval_expr st a e1,eval_expr st a e2)  with
        (Int n1,Int n2) -> Bool(n1 <= n2)
      | _ -> raise (TypeError "Leq")
    )          
  | Le(e1,e2) -> (match (eval_expr st a e1,eval_expr st a e2)  with
        (Int n1,Int n2) -> Bool(n1 < n2)
      | _ -> raise (TypeError "Le")
    )          
  | Geq(e1,e2) -> (match (eval_expr st a e1,eval_expr st a e2)  with
        (Int n1,Int n2) -> Bool(n1 >= n2)
      | _ -> raise (TypeError "Geq")
    )          
  | Ge(e1,e2) -> (match (eval_expr st a e1,eval_expr st a e2)  with
        (Int n1,Int n2) -> Bool(n1 > n2)
      | _ -> raise (TypeError "Ge")
    )          

let eval_var_decls (vdl : var_decl list) (e : env): env =
  List.fold_left
    (fun acc vd ->
      match vd with
        | IntVar x  -> bind acc x (Int 0)
        | BoolVar x -> bind acc x (Bool false)
        | AddrVar x -> bind acc x (Addr "0")
    )
    e
    vdl

(******************************************************************************)
(*                       Small-step semantics of commands                     *)
(******************************************************************************)

let rec trace1_cmd = function
    St _ -> raise NoRuleApplies
  | Cmd(c,st,a) -> (match c with
    | Skip -> St st
    | Assign(x,e) -> (
        (* first tries to update environment if x is bound there *)
        try 
          St (update_env st x (eval_expr st a e)) 
        (* if not, tries to update storage of a *)
        with _ -> 
          St (update_storage st a x (eval_expr st a e)))
    | Seq(c1,c2) -> (match trace1_cmd (Cmd(c1,st,a)) with
          St st1 -> Cmd(c2,st1,a)
        | Cmd(c1',st1,a) -> Cmd(Seq(c1',c2),st1,a))
    | If(e,c1,c2) -> (match eval_expr st a e with
          Bool true -> Cmd(c1,st,a)
        | Bool false -> Cmd(c2,st,a)
        | _ -> failwith("if: type error"))
    | Req(_) -> failwith ("TODO")
    | Send(_,_,_) -> failwith ("TODO")
    | Call(_,_) -> failwith "TODO"
    | Block(vdl,c) ->
      let e = topenv st in
      let e' = eval_var_decls vdl e in
      Cmd(ExecBlock c, { st with stackenv = e'::st.stackenv} , a)
    | ExecBlock(c) -> (match trace1_cmd (Cmd(c,st,a)) with
        | St st -> St { st with stackenv = popenv st }
        | Cmd(c1',st1,a') -> Cmd(ExecBlock(c1'),st1,a'))
    | _ -> failwith "TODO")

(* (match (topenv st f,eval_expr st e) with
          (IProc(a,c),Int n) ->
          let l = getloc st in
          let env' = bind (topenv st) x (IVar l) in
          let mem' = bind (getmem st) l n in
          let st' = (env'::(getenv st), mem', l+1) in
          Cmd(CallExec(c),st')
        | _ -> raise (TypeError "Call of a non-procedure"))
*)
(*                    
    | CallExec(c) -> (match trace1_cmd (Cmd(c,st,a)) with
          St st' -> St (popenv st', getmem st', getloc st',a)
        | Cmd(c',st') -> Cmd(CallExec(c'),st',a))
*)

(*
let sem_decl (e,l) = function
  | IntVar(x) ->  let e' = bind e x (IVar l) in (e',l+1)
  | Constr(f,a,c) -> let e' = bind e f (IProc(a,c)) in (e',l)                                                
  | Proc(f,a,c) -> let e' = bind e f (IProc(a,c)) in (e',l)
*)

let rec trace_rec_cmd n t =
  if n<=0 then [t]
  else try
      let t' = trace1_cmd t
      in t::(trace_rec_cmd (n-1) t')
    with NoRuleApplies -> [t]

  
let init_storage (Contract(_,vdl,_)) : ide -> exprval =
  List.fold_left (fun acc var -> 
      let (x,v) = (match var with 
        | IntVar x  -> (x, Int 0)
        | BoolVar x -> (x, Bool false)
        | AddrVar x -> (x, Addr "0"))
      in bind acc x v) botenv vdl 

let init_sysstate = { 
    accounts = (fun a -> failwith ("account " ^ a ^ " unbound")); 
    stackenv = [botenv];
    active = []; 
}

let trace_cmd n_steps (c:cmd) (a:addr) (st : sysstate)=
  trace_rec_cmd n_steps (Cmd(c,st,a))


(******************************************************************************)
(*                       Small-step semantics of transactions                 *)
(******************************************************************************)

let faucet (a : addr) (n : int) (st : sysstate) : sysstate = 
  if exists_account st a then 
    let as' = { (st.accounts a) with balance = n + (st.accounts a).balance } in
    { st with accounts = bind st.accounts a as' }
  else
    let as' = { balance = n; storage = botenv; code = None; } in
    { st with accounts = bind st.accounts a as'; active = a::st.active }

let find_fun (Contract(_,_,fdl)) (f : ide) : fun_decl option =
  List.fold_left 
  (fun acc (Proc(g,al,c,m)) -> if acc <> None || g<>f then acc else Some (Proc(g,al,c,m)))
  None
  fdl

(* TODO: we should execute constructor!! *)

let deploy_contract (st : sysstate) (a : addr) (c : contract) : sysstate =
  if exists_account st a then failwith ("deploy_contract: address " ^ a ^ " already bound in sysstate")
  else
    let as' = bind st.accounts a ({ balance=0; storage = init_storage c; code = Some c }) in
  { st with accounts = as'; active = a::st.active }

let exec_tx (n_steps : int) (Tx(a,b,f,_)) (st : sysstate) =
  if not (exists_account st a) then failwith ("sender address " ^ a ^ " does not exist") else
  if not (exists_account st b) then failwith ("to address " ^ b ^ " does not exist") else
  let b_state = st.accounts b in match b_state.code with
  | None -> failwith "Call not to a contract"
  | Some src -> (match find_fun src f with
      | None -> failwith ("Contract at address " ^ b ^ " has no function named " ^ f)
      | Some (Proc(_,_,c,_)) ->  
          trace_cmd n_steps c b st)


(*
let trace_tx (n_steps : int) (tx: transaction) (st : sysstate) = 
  Tx(a,c,f,args)
*)

(**********************************************************************
 trace : int -> contract -> conf list

 Usage: trace n c performs n steps of the small-step semantics
 **********************************************************************)

(*
let trace n (Contract(d)) =
  let (e,l) = sem_decl (botenv,0) d
  in trace_rec n (Cmd(c,([e],botmem,l)))
*)

(*
let trace n t = trace_rec n (Cmd(t,bot))
*)


(*
let trace_tx n_steps c =
  trace_rec_cmd n_steps (Cmd(c,[botstorage,botenv],a)))
*)