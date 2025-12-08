open TinysolLib.Ast
open TinysolLib.Utils

(********************************************************************************
 test_parse_cmd : (command, expected AST)
 ********************************************************************************)

let test_parse_cmd (c : string) (t : cmd) =
  c
  |> parse_cmd
  |> blockify_cmd
  |> fun x -> x = t

let%test "test_parse_cmd_1" = test_parse_cmd
  "skip;" 
  Skip  

let%test "test_parse_cmd_2" = test_parse_cmd
  "x=51;"
  (Assign("x",IntConst 51))  

let%test "test_parse_cmd_3" = test_parse_cmd
  "{ int x; x=51; }" 
  (Block([VarT(IntBT,false),"x"],Assign("x",IntConst 51)))  

let%test "test_parse_cmd_4" = test_parse_cmd
  "{ int x; x=51; x=x+1; }"
  (Block([VarT(IntBT,false),"x"],Seq(Assign("x",IntConst 51),Assign("x",Add(Var "x",IntConst 1)))))  

let%test "test_parse_cmd_5" = test_parse_cmd
  "{ int x; x=51; x=x+1; skip; }" 
  (Block([VarT(IntBT,false),"x"],
    Seq(
      Assign("x",IntConst 51),
      Seq(
        Assign("x",Add(Var "x",IntConst 1)),
        Skip
      )
    )))

let%test "test_parse_cmd_6" = test_parse_cmd
  "{ uint x; mapping (uint => int) m; x = m[x+1]; }"
  (Block ([(VarT(UintBT,false), "x"); (MapT (UintBT, IntBT), "m")],
  Assign ("x", MapR (Var "m", Add (Var "x", IntConst 1)))))

let%test "test_parse_cmd_7" = test_parse_cmd
  "{ mapping (uint => uint) m; m[0] = m[0]+1; }"
  (Block ([(MapT (UintBT, UintBT), "m")],
 MapW ("m", IntConst 0, Add (MapR (Var "m", IntConst 0), IntConst 1))))

let%test "test_parse_cmd_8" = test_parse_cmd
  "{ mapping (uint => uint) m; m[m[0]] = m[m[1]+2]+3; }"
  (Block ([(MapT (UintBT, UintBT), "m")],
 MapW ("m", MapR (Var "m", IntConst 0),
  Add (MapR (Var "m", Add (MapR (Var "m", IntConst 1), IntConst 2)),
   IntConst 3))))

let%test "test_parse_cmd_9" = test_parse_cmd
  "x = (true)?1:0;"
  (Assign ("x", IfE (BoolConst true, IntConst 1, IntConst 0)))

let%test "test_parse_cmd_10" = test_parse_cmd
  "x = (true)?(false)?0:1:2;"
  (Assign ("x", IfE (BoolConst true, IfE (BoolConst false, IntConst 0, IntConst 1), IntConst 2)))

let%test "test_parse_cmd_11" = test_parse_cmd
  "x = (true)?0:(false)?0:1;"
  (Assign ("x", IfE (BoolConst true, IntConst 0, IfE (BoolConst false, IntConst 0, IntConst 1))))

let%test "test_parse_contract_1" = try 
  let _ = parse_contract
    "contract C {
        function f(mapping (uint => uint) m) public { m[0] = 1; }
    }"
  in false 
  with _ -> true

let%test "test_parse_contract_2" = try
  parse_contract
  "contract C { uint x; function f() public { x = block.number; } }"
  = 
  (Contract ("C", [], [(VarT(UintBT,false), "x")],
  [Proc ("f", [], Assign ("x", BlockNum), Public, false, None)]))
  with _ -> false

let%test "test_parse_contract_3" = try 
  let _ = parse_contract
    "contract C {
        int immutable x;
        function f(int y) public { y=x; }
    }"
  in true 
  with _ -> false

let%test "test_parse_contract_4" = try 
  let _ = parse_contract
    "contract C {
        int x;
        function f(int immutable y) public { x=y; }
    }"
  in false 
  with _ -> true

let%test "test_parse_contract_5" = try 
  parse_contract
  "contract C { address payable a; function f() public payable { a.transfer(address(this).balance); } }"
  =
  (Contract ("C", [], [(VarT (AddrBT true, false), "a")],
  [Proc ("f", [], Send (Var "a", BalanceOf (AddrCast This)), Public, true, None)]))
  with _ -> false

let%test "test_parse_contract_6" = try 
  parse_contract
  "contract C { function f(address payable a) public payable { a.transfer(address(this).balance); } }"
  =
  (Contract ("C", [], [],
 [Proc ("f", [(VarT (AddrBT true, false), "a")],
   Send (Var "a", BalanceOf (AddrCast This)), Public, true, None)]))
  with _ -> false

let%test "test_parse_contract_7" = try
  parse_contract 
  "contract C { enum State {IDLE, REQ} uint x; function f() public { x = x+1; } }"
  =
  (Contract ("C", [Enum ("State", ["IDLE"; "REQ"])],
  [(VarT (UintBT, false), "x")],
  [Proc ("f", [], Assign ("x", Add (Var "x", IntConst 1)), Public, false, None)]))
  with _ -> false

let%test "test_parse_contract_8" = try
  parse_contract
  "contract C { enum State {IDLE, REQ} State s; function f() public { s = State.REQ; } }"
  =
  (Contract ("C", [Enum ("State", ["IDLE"; "REQ"])],
 [(VarT (CustomBT "State", false), "s")],
 [Proc ("f", [], Assign ("s", EnumOpt ("State", "REQ")), Public, false, None)]))
  with _ -> false
