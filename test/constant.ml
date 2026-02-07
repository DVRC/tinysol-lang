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

(* += on a constant must fail *)
let%test "test_typecheck_constant_5" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() { }
    function f(int n) external { N+=1; }
  }"
  false

(* -= on a constant in the constructor must fail *)
let%test "test_typecheck_constant_6" = test_typecheck
  "contract C {
    int constant N=1;
    constructor() { N-=1; }
    function f(int n) external { }
  }"
  false

(* reading a constant is allowed *)
let%test "test_typecheck_constant_7" = test_typecheck
  "contract C {
    int constant N=1;
    int y;
    constructor() { }
    function f(int n) external { y = N; }
  }"
  true

(* constant and immutable coexist: immutable assignable in constructor, constant never *)
let%test "test_typecheck_constant_8" = test_typecheck
  "contract C {
    int constant N=1;
    int immutable M;
    constructor() { M = 5; }
    function f(int n) external { int x; x = N + M; }
  }"
  true

(* constant and immutable coexist: assigning to constant must fail even with immutable present *)
let%test "test_typecheck_constant_9" = test_typecheck
  "contract C {
    int constant N=1;
    int immutable M;
    constructor() { M = 5; N = 2; }
    function f(int n) external { }
  }"
  false
