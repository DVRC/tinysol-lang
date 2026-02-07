open TinysolLib.Utils
open TinysolLib.Typechecker
open Semantics

let test_typecheck (src: string) (exp : bool)=
  let c = src |> parse_contract |> preprocess_contract in
  match typecheck_contract c with
    | Ok() -> exp
    | _ -> not exp

(* Issue #12 - Example 1: Arity mismatch - function returns 2 values but destructuring expects 3 *)
let%test "test_multiple_return_arity_mismatch" = test_typecheck
  "contract C {
    int x;
    bool b;
    function f() public view returns (int,bool) { return(x,b); }
    function g() public { int w; int y; bool z; (w,y,z) = this.f(); x +=y; b = !z; }
  }"
  false

(* Issue #12 - Example 2: Type mismatch - first variable is bool but function returns int *)
let%test "test_multiple_return_type_mismatch" = test_typecheck
  "contract C {
    int x;
    bool b;
    function f() public view returns (int,bool) { return(x,b); }
    function g() public { bool y; bool z; (y,z) = this.f(); }
  }"
  false

(* Valid case: Correct arity and types *)
let%test "test_multiple_return_valid" = test_typecheck
  "contract C {
    int x;
    bool b;
    function f() public view returns (int,bool) { return(x,b); }
    function g() public { int y; bool z; (y,z) = this.f(); x = y; }
  }"
  true

(* Test return type mismatch *)
let%test "test_return_type_mismatch" = test_typecheck
  "contract C {
    int x;
    function f() public view returns (bool) { return(x); }
  }"
  false

(* Test return arity mismatch *)
let%test "test_return_arity_mismatch" = test_typecheck
  "contract C {
    int x;
    bool b;
    function f() public view returns (int) { return(x,b); }
  }"
  false

(* Basic decons: return two state variables and assign them to locals *)
let%test "test_decons_runtime" = test_exec_tx
  "contract C {
    int x;
    bool b;
    constructor() { x = 42; b = true; }
    function f() public view returns (int,bool) { return(x,b); }
    function g() public { int y; bool z; (y,z) = this.f(); x = y + 1; }
  }"
  ["0xA:0xC.g()"]
  ["x==43"]

(* Decons assigning both values, using only the first *)
let%test "test_decons_use_first_only" = test_exec_tx
  "contract C {
    int x;
    bool b;
    constructor() { x = 10; b = false; }
    function f() public view returns (int,bool) { return(x,b); }
    function g() public { int y; bool z; (y,z) = this.f(); x = y + 1; }
  }"
  ["0xA:0xC.g()"]
  ["x==11"]

(* Return three values *)
let%test "test_decons_three_values" = test_exec_tx
  "contract C {
    int a;
    int b;
    bool c;
    constructor() { a = 1; b = 2; c = true; }
    function f() public view returns (int,int,bool) { return(a,b,c); }
    function g() public { int x; int y; bool z; (x,y,z) = this.f(); a = x + y; }
  }"
  ["0xA:0xC.g()"]
  ["a==3"]

(* Multiple calls: two separate decons in sequence *)
let%test "test_decons_sequential" = test_exec_tx
  "contract C {
    int x;
    bool b;
    constructor() { x = 5; b = true; }
    function f() public view returns (int,bool) { return(x,b); }
    function g() public {
      int y; bool z;
      (y,z) = this.f();
      x = y + 10;
      (y,z) = this.f();
      x = y + 100;
    }
  }"
  ["0xA:0xC.g()"]
  ["x==115"]

(* Return with computed expressions *)
let%test "test_return_computed_exprs" = test_exec_tx
  "contract C {
    int x;
    constructor() { x = 3; }
    function f() public view returns (int,int) { return(x+1, x*2); }
    function g() public { int a; int b; (a,b) = this.f(); x = a + b; }
  }"
  ["0xA:0xC.g()"]
  ["x==10"]
