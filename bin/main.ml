open TinysolLib.Utils
open TinysolLib.Main
       
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

match Array.length(Sys.argv) with
(* trace n / read input from stdin *) 
  2 when Sys.argv.(1)="parse_cmd" -> (match read_line() with
      Some s when s<>"" -> s |> parse_cmd |> string_of_cmd |> print_string
    | _ -> print_newline())
(* trace cms / read cmd and n_steps from stdin *)
| 3 when Sys.argv.(1)="exec_cmd" -> (match read_line() with
    | Some s when s<>"" -> s |> parse_cmd 
      |> fun c -> trace_cmd (int_of_string Sys.argv.(2)) c "0xCAFE" init_sysstate
      |> string_of_trace |> print_string
    | _ -> print_newline())
(* trace1 / read input from file *) 
| 3 when Sys.argv.(1)="parse_contract" -> (match read_file Sys.argv.(2) with
      "" -> print_newline()
    | s -> s |> parse_contract |> string_of_contract |> print_string)
| 3 when Sys.argv.(1)="exec_tx" -> (match read_file Sys.argv.(2) with
      "" -> print_newline()
    | src -> src |> parse_contract
      |> deploy_contract init_sysstate "0xAA" 
      |> faucet "0x0" 100
      |> exec_tx 1000 (Tx("0x0","0xAA","g", []))
      |> string_of_trace |> print_string)
(* wrong usage *)      
| _ -> print_string "Usage:
  dune exec tinysol parse_cmd   : parses cmd in stdin
  dune exec tinysol exec_cmd <n_steps>   : executes n_steps of cmd in stdin
  dune exec tinysol parse_contract <file>   : parses contract in file
  dune exec tinysol exec_tx <file> <n_steps> : executes n_steps of contract in file
"
