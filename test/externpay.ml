open Semantics
open Typecheck

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
