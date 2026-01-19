open Semantics

let%test "test_receive_1" = test_exec_fun
  "contract C {
      uint x;
      receive() external payable { x += 1; }
  }"
  "contract D {
      constructor() payable { }
      function f(address a) public { payable(a).transfer(1); }
  }"
  ["0xA:0xD.f(\"0xC\")"]
  [("0xC","this.balance==1 && x==1"); ("0xD","this.balance==99")]

let%test "test_receive_2" = test_exec_fun
  "contract C {
      D d;
      constructor() { d = \"0xD\"; }
      receive() external payable { d.g(); }
  }"
  "contract D {
      uint x;
      constructor() payable { }
      function f(address a) public { payable(a).transfer(1); }
      function g() public { x += 1; }
  }"
  ["0xA:0xD.f(\"0xC\")"]
  [("0xC","this.balance==1"); ("0xD","this.balance==99 && x==0")]
