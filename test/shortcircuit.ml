open Semantics
(* open Typechecker *)

(*~ Short circuit tests *)
let%test "test_shortcut_1" = test_exec_tx
  "contract C {
      uint x;
      function f() public { if (x==1 || this.g()==1) x+=1; else x=5; }
      function g() public returns(uint) { require(x==0); return 1; }
  }"
  ["0xA:0xC.f()"]
  [("x==1");]

(*~ Consecutive calls ? *)
let%test "test_shortcut_2" = test_exec_tx
  "contract C {
      uint x;
      function f() public { if (x==1 || this.g()==1) x+=1; else x=5; }
      function g() public returns(uint) { require(x==0); return 1; }
  }"
  ["0xA:0xC.f()"; "0xA:0xC.f()"]
  [("x==2");]

let%test "test_shortcut_3" = test_exec_tx
  "contract C {
      uint x;
      function f() public { if (x==0 && this.g()==1) x=1; else x=5; }
      function g() public returns(uint) { require(x==0); return 1; }
  }"
  ["0xA:0xC.f()"]
  [("x==1");]

let%test "test_shortcut_4" = test_exec_tx
  "contract C {
      uint x;
      function f() public { if (x==0 && this.g()==1) x=1; else x=5; }
      function g() public returns(uint) { require(x==0); return 1; }
  }"
  ["0xA:0xC.f()"; "0xA:0xC.f()"]
  [("x==5");]
