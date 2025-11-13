open TinysolLib.Types       
open TinysolLib.Prettyprint
open TinysolLib.Main


let test_parse_cmd (cmd,t) =
  cmd
  |> parse_cmd
  |> fun x -> x = t

(********************************************************************************
 test_trace_cmd : (command, n_steps, variable, expected value after n_steps)
 ********************************************************************************)

let test_trace_cmd (cmd,n_steps,var,exp_val) =
  cmd
  |> parse_cmd
  |> fun c -> last (trace_cmd n_steps c "0xCAFE")
  |> fun t -> match t with
  | St st -> lookup st "0xCAFE" var = exp_val
  | Cmd(_,st,_) -> lookup st "0xCAFE" var = exp_val

let%test "test_parse_cmd_1" = test_parse_cmd
  ("skip;", Skip)  

let%test "test_parse_cmd_2" = test_parse_cmd
  ("x=51;", Assign("x",IntConst 51))  

let%test "test_parse_cmd_3" = test_parse_cmd
  ("{ int x; x=51; }", Block([IntVar "x"],Assign("x",IntConst 51)))  

let%test "test_parse_cmd_4" = test_parse_cmd
  ("{ int x; x=51; x=x+1; }", Block([IntVar "x"],Seq(Assign("x",IntConst 51),Assign("x",Add(Var "x",IntConst 1)))))  

let%test "test_parse_cmd_5" = test_parse_cmd
  ("{ int x; x=51; x=x+1; skip; }", 
    Block([IntVar "x"],
    Seq(
      Assign("x",IntConst 51),
      Seq(
        Assign("x",Add(Var "x",IntConst 1)),
        Skip
      )
    )))  

let%test "test_trace_cmd_1" = test_trace_cmd
  ("{ int x; x=51; skip; }", 2, "x", Int 51)  

let%test "test_trace_cmd_2" = test_trace_cmd
  ("{ int x; x=51; x=x+1; skip; }", 3, "x", Int 52)  

let%test "test_trace_cmd_3" = test_trace_cmd
  ("{ int x; x=51; { int x; x=42; } x=x+1; skip; }", 5, "x", Int 52)  

let%test "test_trace_cmd_4" = test_trace_cmd
  ("{ int x; x=51; { int x; x=42; skip; skip; skip; } x=x+1; skip; }", 5, "x", Int 42)  

let%test "test_trace_cmd_5" = test_trace_cmd
  ("{ int x; x=51; { int x; x=x+1; skip; skip; skip; } x=x+1; skip; }", 5, "x", Int 1)  

let%test "test_trace_cmd_6" = test_trace_cmd
  ("{ int x; x=51; { int x; x=1; } { int x; x=x+3; } x=x+5; skip; }", 7, "x", Int 56)  

let%test "test_trace_cmd_7" = test_trace_cmd
  ("{ int x; x=51; { int y; y=x+1; skip; } { x = 0; } x=x+5; skip; }", 7, "x", Int 5)  

let%test "test_trace_cmd_8" = test_trace_cmd
  ("{ int x; x=51; { int y; y=x+1; skip; } { int x; x = 0; } x=x+5; skip; }", 8, "x", Int 56)  

let%test "test_trace_cmd_9" = test_trace_cmd
  ("{ bool b; b = 2==2; skip; }", 2, "b", Bool true)  

let%test "test_trace_cmd_10" = test_trace_cmd
  ("{ int x; if (x==0) x=1; else x=2; skip; }", 3, "x", Int 1)  

let%test "test_trace_cmd_11" = test_trace_cmd
  ("{ int x; if (x>0) x=1; else x=2; skip; }", 3, "x", Int 2)  

let%test "test_trace_cmd_12" = test_trace_cmd
  ("{ int x; bool b; if (b) x=2; else b=true; skip; }", 2, "x", Int 0)  
