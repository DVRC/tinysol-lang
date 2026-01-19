open Typechecker

let%test "test_typecheck_constant_1" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() { }
    function f(int n) external { }
  }"
  true

let%test "test_typecheck_constant_2" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() { }
    function f(int n) external { N=2; }
  }"
  false

let%test "test_typecheck_constant_3" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() { N=2; }
    function f(int n) external { }
  }"
  false

let%test "test_typecheck_constant_4" = test_typecheck
  "contract C {
    int constant N;
    constructor() { }
    function f(int n) external { }
  }"
  false
