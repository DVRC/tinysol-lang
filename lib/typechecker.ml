open Ast
open Utils
open Prettyprint

(* Types for expressions are a refinement of variable declaration types,
 * since we want to give more specific types to integer constants in order
 * to have a smoother treatment of int and uint types
 *)
type exprtype =
  | BoolConstET of bool
  | BoolET
  | IntConstET of int
  | IntET
  | UintET
  | AddrET of bool
  | EnumET of ide
  | ContractET of ide
  | MapET of exprtype * exprtype

let rec string_of_exprtype = function
  | BoolConstET b   -> "bool " ^ (if b then "true" else "false")
  | BoolET          -> "bool"
  | IntConstET n    -> "int " ^ string_of_int n
  | IntET           -> "int"
  | UintET          -> "uint"
  | AddrET p        -> "address" ^ (if p then " payable" else "")
  | EnumET x        -> x
  | ContractET x    -> x
  | MapET(t1,t2)    -> string_of_exprtype t1 ^ " => " ^ string_of_exprtype t2

(* the result of the contract typechecker is either:
  - Ok():       if all static checks passed
  - Error log:  if some checks did not pass; the log collects all the errors found
 *)
type typecheck_result = (unit, exn list) result

(* >> merges two contract typechecker results *)
let (>>) (out1: typecheck_result) (out2: typecheck_result): typecheck_result =
  match out1 with
  | Ok () -> out2
  | Error log1 -> match out2 with
    | Ok () -> Error log1
    | Error log2 -> Error (log1 @ log2)

(* the result of the expression typechecker is either:
 - Ok(t):      if all static checks passed, and t is the inferred type of the expression
 - Error log:  if some checks did not pass; the log collects all the errors found
 *)

type typecheck_expr_result = (exprtype,exn list) result

(* >>+ merges two expression typechecker results *)
let (>>+) (out1: typecheck_expr_result) (out2: typecheck_expr_result):
  typecheck_expr_result =
  match out1,out2 with
  | Ok _, Ok _ -> assert(false) (* should not happen *)
  | Ok _, Error log2 -> Error log2
  | Error log1, Ok _ -> Error log1
  | Error log1, Error log2 -> Error (log1 @ log2)

(* boring cast from expression typechecker result to contract typechecker result*)
let typeckeck_result_from_expr_result (out: typecheck_expr_result):
  typecheck_result =
  match out with
  | Error log -> Error log
  | Ok(_) -> Ok()

(* The following exceptions represent the all possible errors detected by the typechecker *)
exception TypeError of ide * expr * exprtype * exprtype
exception NotMapError of ide * expr
exception ImmutabilityError of ide * ide
exception UndeclaredVar of ide * ide
exception MultipleDecl of ide
exception MultipleLocalDecl of ide * ide
exception EnumNameNotFound of ide * ide
exception EnumOptionNotFound of ide * ide * ide
exception EnumDupName of ide
exception EnumDupOption of ide * ide
exception MapInLocalDecl of ide * ide

(*~ Adding new exceptions, since that I need *)
exception NonEmptyArgs of ide
exception NonEmptyReturn of ide
exception NotExternPay of ide * visibility_t * fun_mutability_t

(* Multiple return values support (Issue #12) *)
exception DeconsArityMismatch of ide * int * int
exception DeconsTypeMismatch of ide * ide * exprtype * base_type
exception DeconsNonFunCall of ide
exception FunctionNotFound of ide * ide
exception ReturnArityMismatch of ide * int * int
exception ReturnTypeMismatch of ide * int * exprtype * base_type

(*~ Construct a string with the function name and debug infor *)
let logfun f s = "(" ^ f ^ ")\t" ^ s

(* Convert base_type to string for error messages *)
let string_of_base_type = function
  | IntBT -> "int"
  | UintBT -> "uint"
  | BoolBT -> "bool"
  | AddrBT true -> "address payable"
  | AddrBT false -> "address"
  | EnumBT x -> x
  | ContractBT x -> x
  | UnknownBT x -> "unknown:" ^ x

(* Prettyprinting of typechecker errors *)
let string_of_typecheck_error = function
  | TypeError (f,e,t1,t2) ->
    logfun f
      "expression " ^ (string_of_expr e) ^
    " has type " ^ string_of_exprtype t1 ^
    " but is expected to have type " ^ string_of_exprtype t2
  | NotMapError (f,e) -> logfun f (string_of_expr e) ^ " is not a mapping"
  | ImmutabilityError (f,x) ->
    logfun f "variable " ^ x ^ " was declared as immutable, but is used as mutable"
  | UndeclaredVar (f,x) -> logfun f "variable " ^ x ^ " is not declared"
  | MultipleDecl x -> "variable " ^ x ^ " is declared multiple times"
  | MultipleLocalDecl (f,x) ->
    logfun f "variable " ^ x ^ " is declared multiple times"
  | EnumNameNotFound (f,x) -> logfun f "enum ^ " ^ x ^ " is not declared"
  | EnumOptionNotFound (f,x,o) ->
    logfun f "enum option " ^ o ^ " is not found in enum " ^ x
  | EnumDupName x -> "enum " ^ x ^ " is declared multiple times"
  | EnumDupOption (x,o) ->
    "enum option " ^ o ^ " is declared multiple times in enum " ^ x
  | MapInLocalDecl (f,x) ->
    logfun f "mapping " ^ x ^ " not admitted in local declaration"
  | NotExternPay (f, _, _) -> logfun f "must be declared as \"external payable\" "
  | NonEmptyArgs (f) -> logfun f "mustn't have arguments"
  | NonEmptyReturn (f) -> logfun f "must not return anything"
  | DeconsArityMismatch (f, expected, actual) ->
    logfun f "destructuring expects " ^ string_of_int expected ^
    " values but function returns " ^ string_of_int actual
  | DeconsTypeMismatch (f, var, actual, expected) ->
    logfun f "variable " ^ var ^ " has type " ^ string_of_exprtype actual ^
    " but function returns " ^ string_of_base_type expected
  | DeconsNonFunCall (f) ->
    logfun f "destructuring assignment must use a function call"
  | FunctionNotFound (contract, fname) ->
    "function " ^ fname ^ " not found in contract " ^ contract
  | ReturnArityMismatch (f, expected, actual) ->
    logfun f "function should return " ^ string_of_int expected ^
    " values but returns " ^ string_of_int actual
  | ReturnTypeMismatch (f, pos, actual, expected) ->
    logfun f "return value at position " ^ string_of_int pos ^
    " has type " ^ string_of_exprtype actual ^
    " but should be " ^ string_of_base_type expected
  | ex -> Printexc.to_string ex

let exprtype_of_decltype = function
  | IntBT         -> IntET
  | UintBT        -> UintET
  | BoolBT        -> BoolET
  | AddrBT(b)     -> AddrET(b)
  | EnumBT _      -> UintET
  | ContractBT x  -> ContractET x
  | UnknownBT _   -> assert(false) (* should not happen after preprocessing *)

(* Convert base_type to exprtype for return type validation *)
let exprtype_of_basetype = function
  | IntBT -> IntET
  | UintBT -> UintET
  | BoolBT -> BoolET
  | AddrBT p -> AddrET p
  | EnumBT x -> EnumET x
  | ContractBT x -> ContractET x
  | UnknownBT _ -> failwith "UnknownBT should be resolved in preprocessing"

(* typechecker functions take as input the list of variable declarations:
  - var_decl:       state variables
  - local_var_decl: local variables
  The type all_var_decls encapsulates the list of these variables.
*)

type all_var_decls = (var_decl list) * (local_var_decl list)

let get_state_var_decls (avdl : all_var_decls) : var_decl list = fst avdl
let get_local_var_decls (avdl : all_var_decls) : local_var_decl list = snd avdl

(* merges a list of state variable decls and a list of local variable decls *)
let merge_var_decls (vdl: var_decl list) (lvdl: local_var_decl list) : all_var_decls =
  vdl , lvdl

(* adds a list of local variables to all_var_decls *)
let push_local_decls ((vdl: var_decl list), (old_lvdl: local_var_decl list)) new_lvdl =
  (vdl, new_lvdl @ old_lvdl)

(*~ How lookup works:
 *)
let lookup_type (x: ide) (avdl: all_var_decls): exprtype option =
  if x = "msg.sender" then
    Some (AddrET false)
  else if x = "msg.value" then
    Some UintET
  else
  (* first lookup the local variables *)
  avdl
  |> get_local_var_decls
  |> List.map (fun (vd : local_var_decl) -> match vd.ty with
    | VarT(t)   -> (exprtype_of_decltype t),vd.name
    | MapT(tk,tv) -> MapET(exprtype_of_decltype tk, exprtype_of_decltype tv), vd.name)
  |> List.fold_left (fun acc (t,y) -> if acc = None && x = y then Some t else acc) None
  |> fun res -> match res with
    | Some t -> Some t
    | None -> avdl (* if not found, lookup the state variables *)
      |> get_state_var_decls
      |> List.map (fun (vd : var_decl) -> match vd.ty with
        | VarT(t)   -> (exprtype_of_decltype t),vd.name
        | MapT(tk,tv) -> MapET(exprtype_of_decltype tk, exprtype_of_decltype tv), vd.name)
      |> List.fold_left
      (fun acc (t, y) -> if acc = None && x = y then Some t else acc)
      None

(*~ Totally awful name.
 * I thought "dup" was intended in an "imperative" sense.
 * Would make sense naming this as "any_dup" (as existence qualifier)
 *)
let rec dup = function
  | [] -> None
  | x::l -> if List.mem x l then Some x else dup l

(* no_dup_var_decls:
    checks that no variables are declared multiple times
 *)
let no_dup_var_decls vdl =
  vdl
  |> List.map (fun (vd : var_decl) -> vd.name)
  |> dup
  |> fun res -> match res with
    None -> Ok ()
  | Some x -> Error ([MultipleDecl x])

(*~ Changed names for clarity *)
let no_dup_local_var_decls fname vardecl_l =
  vardecl_l
  |> List.map (fun (vd: local_var_decl) -> vd.name)
  |> dup
  |> fun res -> match res with
    None -> Ok ()
  | Some x -> Error ([MultipleLocalDecl (fname ,x)])

let no_dup_fun_decls vdl =
  vdl
  |> List.map (fun fd -> match fd with
    | Constr(_) -> "constructor"
    | Proc(f,_,_,_,_,_) -> f)
  |> dup
  |> fun res -> match res with None -> Ok () | Some x -> Error ([MultipleDecl x])

(* Find function and return its return types for Decons validation *)
let find_function_return_types (fdl: fun_decl list) (fname: ide) : base_type list option =
  List.fold_left (fun acc fd ->
    match fd with
    | Constr _ -> acc
    | Proc(name, _, _, _, _, ret_types) when name = fname -> Some ret_types
    | _ -> acc
  ) None fdl

let subtype t0 t1 = match t1 with
  | BoolConstET _ -> (match t0 with BoolConstET _ -> true | _ -> false)
  | BoolET -> (match t0 with BoolConstET _ | BoolET -> true | _ -> false)
  | IntConstET _ -> (match t0 with IntConstET _ -> true | _ -> false)
  | UintET -> (match t0 with IntConstET n when n>=0 -> true | UintET -> true | _ -> false)
  | IntET -> (match t0 with IntConstET _ | IntET -> true | _ -> false) (* uint is not convertible to int *)
  | AddrET _ -> (match t0 with AddrET _ -> true | _ -> false)
  | _ -> t0 = t1

let rec typecheck_expr (f : ide) (edl : enum_decl list) vdl = function
  | BoolConst b -> Ok (BoolConstET b)

  | IntConst n -> Ok (IntConstET n)

  | IntVal _ | UintVal _ ->
    assert(false) (* these expressions only occur at runtime *)

  | AddrConst _ -> Ok (AddrET false)

  | BlockNum -> Ok(UintET)

  | This -> Ok(AddrET false) (* TODO: check coherence with Solidity *)

  | Var x ->
    (match lookup_type x vdl with
    | Some t -> Ok(t)
    | None -> Error [UndeclaredVar (f,x)])

  | MapR(e1,e2) ->
    (match (typecheck_expr f edl vdl e1, typecheck_expr f edl vdl e2) with
     | Ok(MapET(t1k, t1v)), Ok(t2) when t2 = t1k -> Ok(t1v)
     | Ok(MapET(t1k, _)), Ok(t2) -> Error [TypeError (f,e2,t2,t1k)]
     | _ -> Error [NotMapError(f,e1)])

  | BalanceOf(e) ->
    (match typecheck_expr f edl vdl e with
       Ok(AddrET(_)) -> Ok(UintET)
     | Ok(t) -> Error [TypeError (f,e,t,AddrET(false))]
     | _ as err -> err)

  | Not(e) ->
    (match typecheck_expr f edl vdl e with
     | Ok(BoolConstET b) -> Ok(BoolConstET (not b))
     | Ok(BoolET) -> Ok(BoolET)
     | Ok(t) -> Error [TypeError (f,e,t,BoolET)]
     | _ as err -> err)

  | And(e1,e2) ->
    (match (typecheck_expr f edl vdl e1,typecheck_expr f edl vdl e2) with
     | Ok(BoolConstET false), Ok(t2) when subtype t2 BoolET ->
       Ok(BoolConstET false)
     | Ok(t1), Ok(BoolConstET false) when subtype t1 BoolET ->
       Ok(BoolConstET false)
     | Ok(t1), Ok(t2) when subtype t1 BoolET && subtype t2 BoolET -> Ok(BoolET)
     | Ok(t1), _ when not (subtype t1 BoolET) ->
       Error [TypeError (f,e1,t1,BoolET)]
     | _,Ok(t) -> Error [TypeError (f,e2,t,BoolET)]
     | err1,err2 -> err1 >>+ err2)

  | Or(e1,e2) ->
    (match (typecheck_expr f edl vdl e1,typecheck_expr f edl vdl e2) with
     | Ok(BoolConstET true), Ok(t2) when subtype t2 BoolET ->
       Ok(BoolConstET true)
     | Ok(t1), Ok(BoolConstET true) when subtype t1 BoolET ->
       Ok(BoolConstET true)
     | Ok(t1), Ok(t2) when subtype t1 BoolET && subtype t2 BoolET -> Ok(BoolET)
     | Ok(t1), _ when not (subtype t1 BoolET) ->
       Error [TypeError (f,e1,t1,BoolET)]
     | _, Ok(t2) -> Error [TypeError (f, e2, t2, BoolET)]
     | err1, err2 -> err1 >>+ err2)

  | Add(e1,e2) ->
    (match (typecheck_expr f edl vdl e1, typecheck_expr f edl vdl e2) with
     | Ok(IntConstET n1), Ok(IntConstET n2) -> Ok(IntConstET (n1+n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(UintET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(IntET)
     | Ok(t1),_ when not (subtype t1 IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | _,Ok(t2) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | Sub(e1,e2) ->
    (match (typecheck_expr f edl vdl e1, typecheck_expr f edl vdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(IntConstET (n1-n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(UintET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(IntET)
     | Ok(t1),_ when not (subtype t1 IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | _,Ok(t2) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | Mul(e1,e2) ->
    (match (typecheck_expr f edl vdl e1, typecheck_expr f edl vdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(IntConstET (n1*n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(UintET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(IntET)
     | Ok(t1),_ when not (subtype t1 IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | _,Ok(t2) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | Div(_) -> failwith "Div: TODO"

  | Eq(e1,e2) ->
    (match (typecheck_expr f edl vdl e1,typecheck_expr f edl vdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(BoolConstET (n1 = n2))
     | Ok(t1),Ok(t2) when t1=t2-> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 t2 && subtype t2 t1 ->
       Ok(BoolET) (* AddrET _ *)
     | Ok(t1),Ok(t2) -> Error [TypeError (f,e2,t2,t1)]
     | err1,err2 -> err1 >>+ err2)

  | Neq(e1,e2) ->
    (match (typecheck_expr f edl vdl e1,typecheck_expr f edl vdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(BoolConstET (n1 <> n2))
     | Ok(t1),Ok(t2) when t1=t2-> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 t2 && subtype t2 t1 ->
       Ok(BoolET) (* AddrET _ *)
     | Ok(t1),Ok(t2) -> Error [TypeError (f,e2,t2,t1)]
     | err1,err2 -> err1 >>+ err2)

  | Leq(e1,e2) ->
    (match (typecheck_expr f edl vdl e1,typecheck_expr f edl vdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(BoolConstET (n1 <= n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(BoolET)
     | Ok(t1),Ok(IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | (_,Ok(t2)) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | Lt(e1,e2) ->
    (match (typecheck_expr f edl vdl e1,typecheck_expr f edl vdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(BoolConstET (n1 < n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(BoolET)
     | Ok(t1),Ok(IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | (_,Ok(t2)) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | Geq(e1,e2) ->
    (match (typecheck_expr f edl vdl e1,typecheck_expr f edl vdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(BoolConstET (n1 >= n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(BoolET)
     | Ok(t1),Ok(IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | (_,Ok(t2)) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | Gt(e1,e2) ->
    (match (typecheck_expr f edl vdl e1,typecheck_expr f edl vdl e2) with
     | Ok(IntConstET n1),Ok(IntConstET n2) -> Ok(BoolConstET (n1 > n2))
     | Ok(t1),Ok(t2) when subtype t1 UintET && subtype t2 UintET -> Ok(BoolET)
     | Ok(t1),Ok(t2) when subtype t1 IntET && subtype t2 IntET -> Ok(BoolET)
     | Ok(t1),Ok(IntET) -> Error [TypeError (f,e1,t1,IntET)]
     | (_,Ok(t2)) -> Error [TypeError (f,e2,t2,IntET)]
     | err1,err2 -> err1 >>+ err2)

  | IfE(e1,e2,e3) ->
    (match (typecheck_expr f edl vdl e1,
            typecheck_expr f edl vdl e2,
            typecheck_expr f edl vdl e3)
     with
     | Ok(BoolConstET true),Ok(t2),_ -> Ok(t2)
     | Ok(BoolConstET false),_,Ok(t3) -> Ok(t3)
     | Ok(BoolET),Ok(t2),Ok(t3) when subtype t2 t3 -> Ok(t3)
     | Ok(BoolET),Ok(t2),Ok(t3) when subtype t3 t2 -> Ok(t2)
     | Ok(BoolET),Ok(t2),Ok(t3) -> Error [TypeError (f,e3,t3,t2)]
     | Ok(t1),_,_ -> Error [TypeError (f,e1,t1,BoolET)]
     | err1,err2,err3 -> err1 >>+ err2 >>+ err3)

  | IntCast(e) -> (match typecheck_expr f edl vdl e with
      | Ok(IntConstET _) | Ok(IntET) | Ok(UintET) -> Ok(IntET)
      | Ok(t) -> Error [TypeError (f,e,t,IntET)]
      | err -> err)

  | UintCast(e) -> (match typecheck_expr f edl vdl e with
      | Ok(IntConstET n) when n>=0 -> Ok(IntConstET n)
      | Ok(IntET) | Ok(UintET) -> Ok(UintET)
      | Ok(t) -> Error [TypeError (f,e,t,IntET)]
      | err -> err)

  | AddrCast(e) -> (match typecheck_expr f edl vdl e with
      | Ok(AddrET(b))     -> Ok(AddrET b)
      | Ok(IntConstET _)  -> Ok(AddrET false)
      | Ok(UintET)        -> Ok(AddrET false)
      | Ok(IntET)         -> Ok(AddrET false)
      | Ok(t)             -> Error [TypeError (f,e,t,IntET)]
      | err               -> err)

  | PayableCast(e) -> (match typecheck_expr f edl vdl e with
      | Ok(AddrET _)      -> Ok(AddrET true)
      | Ok(IntConstET 0)  -> Ok(AddrET false)
      | Ok(t)             -> Error [TypeError (f,e,t,IntET)]
      | err               -> err)

  | EnumOpt(enum_name,option_name) ->
      edl
      |> List.filter (fun (Enum(y, _)) -> y = enum_name)
      |> fun edl -> (match edl with [Enum(_, ol)] -> Some ol | _ -> None)
      |> fun l_opt -> (match l_opt with
        | None -> Error [EnumNameNotFound (f,enum_name)]
        | Some ol -> (match find_index (fun o -> o=option_name) ol with
          None -> Error [EnumOptionNotFound(f,enum_name,option_name)]
          | Some i -> Ok(IntConstET i)))

  | EnumCast(x,e) -> (match typecheck_expr f edl vdl e with
      | Ok(IntConstET _) | Ok(UintET) | Ok(IntET) -> Ok(EnumET x)
      | Ok(t) -> Error [TypeError (f,e,t,IntET)]
      | err -> err)

  | ContractCast(x,e) -> (match typecheck_expr f edl vdl e with
      | Ok(AddrET _) -> Ok(ContractET x)
      | Ok(t) -> Error [TypeError (f,e,t,AddrET(false))]
      | err -> err)

  | UnknownCast(_) -> assert(false) (* should not happen after preprocessing *)
  | FunCall(obj, _fname, value, args) ->
    (* Validate FunCall components; full return type checking happens in Decons *)
    (match typecheck_expr f edl vdl obj with
     | Error e -> Error e
     | Ok _ ->
       (match typecheck_expr f edl vdl value with
        | Error e -> Error e
        | Ok _ ->
          (* Validate all arguments *)
          let arg_checks = List.map (typecheck_expr f edl vdl) args in
          let has_errors = List.exists (function Error _ -> true | _ -> false) arg_checks in
          if has_errors then
            (* Collect all errors *)
            List.fold_left (fun acc res ->
              match acc, res with
              | Error e1, Error e2 -> Error (e1 @ e2)
              | Error e, _ -> Error e
              | _, Error e -> Error e
              | Ok _, Ok _ -> Ok IntET
            ) (Ok IntET) arg_checks
          else
            Ok IntET  (* Return a placeholder type since we don't have tuple types *)
       )
    )

  | ExecFunCall(_) -> assert(false) (* this should not happen at static time *)

let is_immutable (x : ide) (vdl : var_decl list) =
  List.fold_left (fun acc (vd : var_decl) ->
      acc || (vd.name = x && vd.mutability <> Mutable)) false vdl

(*~ f is an identifier of the procedure,
 * vdl here is the arguments list (?)
 * In any case, it checks that for each var declaration there aren't
 * variables of mapping type.
 * If there are, it returns an error
 *)
let typecheck_local_decls (f: ide) (vdl: local_var_decl list) = List.fold_left
  (fun acc vd -> match vd.ty with
    | MapT(_) -> acc >> Error [MapInLocalDecl (f, vd.name)]
    | _ -> acc)
  (Ok ())
  vdl

(* Typecheck commands fdl and expected_ret added for multiple return values support *)
let rec typecheck_cmd (f: ide) (edl: enum_decl list) (vdl: all_var_decls) (fdl: fun_decl list) (expected_ret: base_type list option) = function
    | Skip -> Ok ()

    | Assign(x,e) ->
      (* the immutable modifier is not checked for the constructor *)
      if f <> "constructor" && is_immutable x (get_state_var_decls vdl) then
        Error [ImmutabilityError (f,x)]
      else (
        match typecheck_expr f edl vdl e, typecheck_expr f edl vdl (Var x) with
        | Ok(te), Ok(tx) -> if subtype te tx
          then Ok() else Error [TypeError (f,e,te,tx)]
        | res1, res2 -> typeckeck_result_from_expr_result (res1 >>+ res2)
      )

    | Decons(var_list, expr) ->
      (* Validate destructuring: (a, b, c) = this.f() *)
      (match expr with
       | FunCall(obj, fname, value, args) ->
         (match typecheck_expr f edl vdl obj with
          | Error e -> Error e
          | Ok obj_type ->
            (* Validate FunCall components *)
            typeckeck_result_from_expr_result (typecheck_expr f edl vdl value)
            >> List.fold_left (fun acc arg ->
                 acc >> typeckeck_result_from_expr_result (typecheck_expr f edl vdl arg)
               ) (Ok ()) args
            >> (match obj_type with
             | ContractET _ | AddrET _ ->
               (match find_function_return_types fdl fname with
                | None -> Error [FunctionNotFound ("this", fname)]
                | Some ret_types ->
                  (* Check arity *)
                  let expected = List.length ret_types in
                  let actual = List.length var_list in
                  if expected <> actual then
                    Error [DeconsArityMismatch (fname, expected, actual)]
                  else
                    (* Validate types *)
                    let type_checks = List.map2 (fun var_opt ret_type ->
                      match var_opt with
                      | None -> Ok ()
                      | Some var_name ->
                        (match typecheck_expr f edl vdl (Var var_name) with
                         | Ok var_type ->
                           let expected_type = exprtype_of_basetype ret_type in
                           if subtype expected_type var_type then Ok ()
                           else Error [DeconsTypeMismatch (fname, var_name, var_type, ret_type)]
                         | Error e -> Error e
                        )
                    ) var_list ret_types in
                    List.fold_left (>>) (Ok ()) type_checks
               )
             | _ -> Error [DeconsNonFunCall f]  (* Not a contract call *)
            )
         )
       | _ -> Error [DeconsNonFunCall f]
      )

    | MapW(x, ek, ev) ->
        (match typecheck_expr f edl vdl (Var x),
               typecheck_expr f edl vdl ek,
               typecheck_expr f edl vdl ev with
          | Ok(tx), Ok(tk), Ok(tv) -> (match tx with
            | MapET(txk,_) when not (subtype tk txk) ->
              Error [TypeError (f, ek, tk, txk)]
            | MapET(_,txv) when not (subtype tv txv) ->
              Error [TypeError (f, ev, tv, txv)]
            | MapET(_,_) -> Ok()
            | _ -> Error [NotMapError (f, Var x)])
          | res1,res2,res3 -> typeckeck_result_from_expr_result (res1 >>+ res2 >>+ res3))
    | Seq(c1, c2) -> typecheck_cmd f edl vdl fdl expected_ret c1 >> typecheck_cmd f edl vdl fdl expected_ret c2

    | If(e, c1, c2) -> (match typecheck_expr f edl vdl e with
          | Ok(BoolConstET true)  -> typecheck_cmd f edl vdl fdl expected_ret c1
          | Ok(BoolConstET false) -> typecheck_cmd f edl vdl fdl expected_ret c2
          | Ok(BoolET) -> typecheck_cmd f edl vdl fdl expected_ret c1
            >> typecheck_cmd f edl vdl fdl expected_ret c2
          | Ok(te) -> Error [TypeError (f,e,te,BoolET)]
          | res -> typeckeck_result_from_expr_result res)

    | Send(ercv,eamt) -> (match typecheck_expr f edl vdl ercv with
          | Ok(AddrET(true)) -> Ok() (* can only send to payable addresses *)
          | Ok(t_ercv) -> Error [TypeError(f, ercv, t_ercv, AddrET(true))]
          | res -> typeckeck_result_from_expr_result res)
          >> (match typecheck_expr f edl vdl eamt with
          | Ok(t_eamt) when subtype t_eamt UintET -> Ok()
          | Ok(t_eamt) -> Error [TypeError(f, eamt, t_eamt, UintET)]
          | res -> typeckeck_result_from_expr_result res)

    | Req(e) -> (match typecheck_expr f edl vdl e with
          | Ok(BoolET) -> Ok()
          | Ok(te) -> Error [TypeError (f, e, te, BoolET)]
          | res -> typeckeck_result_from_expr_result res)

    | Block(lvdl,c) ->
        typecheck_local_decls f lvdl
        >> let vdl' = push_local_decls vdl lvdl in
        typecheck_cmd f edl vdl' fdl expected_ret c

    | ExecBlock(_) -> assert(false) (* should not happen at static time *)

    | Decl(_) -> assert(false) (* should not happen after blockify *)

    | ProcCall(_) -> failwith "TODO: ProcCall"

    | ExecProcCall(_) -> assert(false) (* should not happen at static time *)

    | Return(expr_list) ->
      (* Validate return statement matches function signature *)
      (match expected_ret with
       | None -> Ok ()  (* Constructor or no return expected *)
       | Some ret_types ->
         (* Validate arity *)
         let expected = List.length ret_types in
         let actual = List.length expr_list in
         if expected <> actual then
           Error [ReturnArityMismatch (f, expected, actual)]
         else
           (* Validate types *)
           let type_checks = List.mapi (fun i (expr, ret_type) ->
             match typecheck_expr f edl vdl expr with
             | Ok expr_type ->
               let expected_type = exprtype_of_basetype ret_type in
               if subtype expr_type expected_type then Ok ()
               else Error [ReturnTypeMismatch (f, i+1, expr_type, ret_type)]
             | Error e -> Error e
           ) (List.combine expr_list ret_types) in
           (* Combine all results *)
           List.fold_left (>>) (Ok ()) type_checks
      )


(*~ Helpers for receive checks. Need more work *)
let ensure_empty_args fname = function
    [] -> Ok ()
  | _ -> Error [NonEmptyArgs (fname)]

let ensure_empty_ret fname = function
    [] -> Ok ()
  | _ -> Error [NonEmptyReturn (fname)]

let ensure_ext_pay fname vis mut = match vis, mut with
    v,m when v = External && m = Payable -> Ok ()
  | _ -> Error [NotExternPay (fname, vis, mut)]

(*~ Typecheck for constructor, function and receive
 * It takes a list of enum declarations, a list of variables and checks
 * for duplicate
 *)
let typecheck_fun (edl : enum_decl list) (vdl : var_decl list) (fdl: fun_decl list) = function
  | Constr (al,c,_) ->
    no_dup_local_var_decls "constructor" al
    >> typecheck_local_decls "constructor" al
    >> typecheck_cmd "constructor" edl (merge_var_decls vdl al) fdl None c
  | Proc ("receive", al, c, vis, mut, ret) -> let f = "receive" in (*~ WIP *)
    ensure_empty_args f al
    >> ensure_ext_pay f vis mut
    >> typecheck_cmd f edl (merge_var_decls vdl al) fdl (Some ret) c
    >> ensure_empty_ret f ret
  | Proc (f,al,c,_,__,ret) ->
    no_dup_local_var_decls f al
    >> typecheck_local_decls f al
    >> typecheck_cmd f edl (merge_var_decls vdl al) fdl (Some ret) c

(* dup_first: finds the first duplicate in a list *)
let rec dup_first (l : 'a list) : 'a option = match l with
  | [] -> None
  | h::tl -> if List.mem h tl then Some h else dup_first tl

let typecheck_enums (edl : enum_decl list) =
  match dup_first (List.map (fun (Enum(x,_)) -> x) edl) with
  | Some x -> Error [EnumDupName x] (* there are two enums with the same name *)
  | None -> List.fold_left (fun acc (Enum(x,ol)) ->
      match dup_first ol with
      | Some o -> acc >> (Error [EnumDupOption (x,o)])
      | None -> acc
    )
    (Ok ())
    edl

(* typecheck_contract : contract -> (unit,string) result
    Perform several static checks on a given contract. The result is:
    - Ok () if all checks succeed
    - Error log otherwise, where log explains the reasons of the failed checks
 *)

let typecheck_contract (Contract(_,edl,vdl,fdl)) : typecheck_result =
  (* no multiply declared enums *)
  typecheck_enums edl
  (* no multiply declared state variables *)
  >> no_dup_var_decls vdl
  (* no multiply declared functions *)
  >> no_dup_fun_decls fdl
  >> List.fold_left (fun acc fd -> acc >> typecheck_fun edl vdl fdl fd) (Ok ()) fdl


let string_of_typecheck_result = function
  Ok() -> "Typecheck ok"
| Error log -> List.fold_left
  (fun acc ex -> acc ^ (if acc="" then "" else "\n") ^ string_of_typecheck_error ex) "" log
