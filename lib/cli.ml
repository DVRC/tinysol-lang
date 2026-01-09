(******************************************************************************)
(*                                    Tinysol CLI                             *)
(******************************************************************************)

open Ast
open Cli_ast
open Utils
open Sysstate
open Main
open Prettyprint

let string_of_cli_cmd = function
  | Faucet(a, n) -> "faucet " ^ a ^ " " ^ string_of_int n
  | Deploy(tx, contract_name, filename) ->
    "deploy " ^ string_of_transaction tx ^ " " ^ contract_name ^ " " ^ filename
  | CallFun tx -> string_of_transaction tx
  | Revert tx -> "revert " ^ string_of_transaction tx
  | Assert(a, e) -> "assert " ^ a ^ " " ^ string_of_expr e
  | LastReverted -> "lastReverted"
  | NotLastReverted -> "!lastReverted"
  | SetBlockNum(n) -> "block.number = " ^ string_of_int n

let is_empty_or_comment (s : string) =
  let len = String.length s in (*~ They didn't teach us about these neat feautures... *)
  let rec skip i = (* skip leading spaces *)
    if i >= len then true                      (* string is only spaces → empty *)
    else if s.[i] = ' ' then skip (i + 1)
    else if i + 1 < len && s.[i] = '/' && s.[i+1] = '/' then true
    else false
  in
  skip 0

(*~ Checks if input is an assert type (?) *)
let is_assert = function
  | Assert _
  | LastReverted
  | NotLastReverted -> true
  | _ -> false

(*~ This function takes a command, a reverted flag and a state, and returns a new state *)
(*~
 * Let me explain interesting[1]:
 * eval_expr takes a record st (state), and the keyword "with" updates the
 * field callstack, which is a list of records where the callee takes "a" and
 * locals (local variables?) is an empty list (I assume either arguments or vars)
 *)
let exec_cli_cmd (cc : cli_cmd) (lastReverted : bool) (st : sysstate) : sysstate =
  match cc with
  | Faucet(a, n) -> faucet a n st
  | Deploy(tx, contract_name, filename) ->
      let src = read_contract_in_file contract_name filename
      in st |> deploy_contract tx src
  | CallFun tx -> st |> exec_tx 1000 tx |> fun res -> (match res with (*~ Funcall with 1000 wei (?) *)
      | Ok st -> st
      | Error msg -> failwith msg)
  | Revert tx ->
      st |> exec_tx 1000 tx |> fun res -> (match res with
        | Ok _ -> failwith ("revert violation: transaction " ^ string_of_transaction tx ^ " should revert")
        | Error _ -> st)
  | Assert(a, e) -> (*~ Interesting[1] *)
    (match eval_expr { st with callstack = [{callee = a; locals = []}] } e with
    | Bool true -> st
    | _ -> failwith ("assertion violation: " ^ string_of_cli_cmd cc))
  | LastReverted -> if lastReverted then st else failwith "lastReverted"
  | NotLastReverted -> if (not lastReverted) then st else failwith "!lastReverted"
  | SetBlockNum(n) -> { st with blocknum = n }

let exec_cli_cmd_list (verbose : bool) (ccl : cli_cmd list) (st : sysstate) =
  List.fold_left
  (fun (lastReverted, sti) cc ->
    if verbose && not (is_assert cc) then
      print_endline (string_of_sysstate [] sti ^ "\n--- " ^ string_of_cli_cmd cc ^ " --->")
    else (); (*~ SEQUENCE CONSTRUCT, back to procedural basics... *)
    try
      (false, exec_cli_cmd cc lastReverted sti)
    with ex ->
      let _ = print_endline (Printexc.to_string ex) in
      (true, sti)
  ) (false, st) ccl
