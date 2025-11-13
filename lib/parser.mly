%{
open Ast
%}

%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token PLUS
%token MINUS
%token MUL
%token EQ
%token LEQ
%token LE
%token GEQ
%token GE
%token <string> ID
%token <string> CONST
%token <string> STRING

%token SKIP
%token TAKES
%token CMDSEP
%token IF
%token ELSE
%token REQ

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token EOF

%token CONTRACT
%token CONSTR
%token FUN
%token INT
%token BOOL
%token ADDR
%token RECEIVESEP
%token SENDSEP
%token TOKSEP
%token ARGSEP

%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left PLUS MINUS
%left TIMES

%left SEQ
%nonassoc ELSE DO

%start <contract> contract
%type <var_decl> var_decl
%type <fun_decl> fun_decl
%type <cmd> cmd
%type <args> args
%type <expr> expr 

%start <cmd> cmd_test

%%

contract:
  | CONTRACT; c=ID; LBRACE; vdl = list(var_decl); fdl = list(fun_decl); RBRACE; EOF { Contract(c,vdl,fdl) }
;

expr:
  | n = CONST { IntConst(int_of_string n) }
  | s = STRING { AddrConst(s) }
  | TRUE { True }
  | FALSE { False }
  | NOT; e=expr { Not e }
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | e1=expr; PLUS; e2=expr { Add(e1,e2) }
  | e1=expr; MINUS; e2=expr { Sub(e1,e2) }
  | e1=expr; MUL; e2=expr { Mul(e1,e2) }
  | e1=expr; EQ; e2=expr { Eq(e1,e2) }
  | e1=expr; LEQ; e2=expr { Leq(e1,e2) }
  | e1=expr; LE; e2=expr { Le(e1,e2) }
  | e1=expr; GEQ; e2=expr { Geq(e1,e2) }
  | e1=expr; GE; e2=expr { Ge(e1,e2) }
  | x = ID { Var(x) }
  | LPAREN; e = expr; RPAREN { e }
;

nonseq_cmd:
  | SKIP; CMDSEP;  { Skip }
  | REQ; e = expr; CMDSEP; { Req(e) } 
  | x = ID; TAKES; e = expr; CMDSEP; { Assign(x,e) }
  | x = ID; SENDSEP; e=expr; TOKSEP; t = ID; CMDSEP; { Send(x,e,t) }
  | f = ID; LPAREN; e=expr; RPAREN; CMDSEP; { Call(f,e) }

cmd:
  | c = nonseq_cmd { c }
  | c1 = cmd; c2 = cmd { Seq(c1,c2) }
  | IF LPAREN; e = expr; RPAREN; c1 = nonseq_cmd ELSE c2 = nonseq_cmd { If(e,c1,c2) }
  | IF LPAREN; e = expr; RPAREN; LBRACE; c1 = cmd; RBRACE; ELSE c2 = nonseq_cmd { If(e,c1,c2) }
  | IF LPAREN; e = expr; RPAREN; c1 = nonseq_cmd; ELSE LBRACE; c2 = cmd; RBRACE; { If(e,c1,c2) }
  | IF LPAREN; e = expr; RPAREN; LBRACE; c1 = cmd; RBRACE; ELSE; LBRACE; c2 = cmd; RBRACE { If(e,c1,c2) }
  | LBRACE; c = cmd; RBRACE { c }
  | LBRACE; vdl = list(var_decl) c = cmd; RBRACE { Block(vdl, c) }
;

var_decl:
  | INT x = ID; CMDSEP { IntVar x }
  | BOOL x = ID; CMDSEP { BoolVar x }
  | ADDR x = ID; CMDSEP { AddrVar x }
;

fun_decl:
  | FUN { Proc("zzz", [], Skip) } 
  | CONSTR; f = ID; LPAREN; a = args; RPAREN; LBRACE; c = cmd; RBRACE { Constr(f,a,c) }
  | FUN; f = ID; LPAREN; a = args; RPAREN; LBRACE; c = cmd; RBRACE { Proc(f,a,c) }
;

cmd_test:
  | c = cmd; EOF { c }
;

args:
  | a = separated_list(ARGSEP, arg) { a } ;

arg:
  | x = ID { IntArg(x) }
  | RECEIVESEP; x = ID; TOKSEP; t = ID; { RcvArg(x,t) }
;
