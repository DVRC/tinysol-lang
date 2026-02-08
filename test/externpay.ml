open Typechecker

(*~ External payable test ? *)
let%test "test_typecheck_receive_1" = test_typecheck
  "contract C {
    receive() external payable { }
  }"
  true

let%test "test_typecheck_receive_2" = test_typecheck
  "contract C {
    receive() public payable { }
  }"
  false

let%test "test_typecheck_receive_3" = test_typecheck
  "contract C {
    receive() external { }
  }"
  false

let%test "test_typecheck_receive_4" = test_typecheck
  "contract C {
    receive(int x) external payable { }
  }"
  false

(*~ Will it work with swapped attributes? *)
let%test "test_typecheck_receive_5" = test_typecheck
  "contract C {
    receive() payable external { }
  }"
  true

(*~ Check for absence of external
 * Fixme: This assert crashes the parser
 *)
(*let%test "test_typecheck_receive_6" = test_typecheck
  "contract C {
    receive() payable { }
  }"
  false*)

(*~ Cannot have returns *)
let%test "test_typecheck_receive_7" = test_typecheck
  "contract C {
    receive() external payable returns(uint) { return 1; }
  }"
  false

let%test "test_typecheck_receive_8" = test_typecheck
  "contract C {
    receive() external payable returns(uint) { }
  }"
  false

(*~ Cannot have return statements *)
let%test "test_typecheck_receive_9" = test_typecheck
  "contract C {
    receive() external payable { return 1; }
  }"
  false
