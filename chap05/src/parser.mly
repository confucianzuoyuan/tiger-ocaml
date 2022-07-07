%{
open Ast
%}

%token <int> INT
%token <string> STRING
%token <string> ID
%token PLUS
%token MINUS
%token DIV
%token TIMES
%token AND
%token OR
%token LT
%token LE
%token GT
%token GE
%token EQ
%token NE
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token TO
%token FOR
%token WHILE
%token DO
%token DOT
%token ASSIGN
%token IF
%token ELSE
%token THEN
%token FUNCTION
%token VAR
%token LET
%token COLON
%token SEMI
%token COMMA
%token NIL
%token END
%token TYPE
%token IN
%token OF
%token ARRAY
%token BREAK
%token EOF

%nonassoc ASSIGN
%nonassoc ID
%nonassoc DO
%nonassoc THEN
%nonassoc ELSE
%nonassoc LBRACKET OF
%left OR
%left AND
%nonassoc EQ NE LT LE GT GE
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS

%start <Ast.exp> program

%%

program:
    | e = exp; EOF { e }
    ;

exp:
    | e = lvalue; { VarExp e }
    | LPAREN; RPAREN; { SeqExp [] }
    | x = ID; LPAREN; funarg = separated_list(COMMA, exp); RPAREN; { CallExp {func=(Symbol.symbol x); args=funarg} }
    | x = ID; LBRACE; recordbody = separated_list(COMMA, field); RBRACE; { RecordExp {fields=recordbody; typ=(Symbol.symbol x)} }
    | x = ID; LBRACKET; e1 = exp; RBRACKET; OF; e2 = exp; { ArrayExp {typ=(Symbol.symbol x); size=e1; init=e2} }
    | LPAREN; e = exp; SEMI; seq = separated_list(SEMI, exp); RPAREN; { SeqExp (e::seq) }
    | LET; d = decs; IN; END; { LetExp {decs=d; body=SeqExp []} }
    | LET; d = decs; IN; letbody = exp; END; { LetExp {decs=d; body=letbody} }
    | LET; d = decs; IN; e = exp; SEMI; et = separated_list(SEMI, exp); END; { LetExp {decs=d; body=SeqExp (e::et)} }
    | NIL; { NilExp }
    | i = INT; { IntExp i }
    | s = STRING; { StringExp s }
    | lhs = exp; PLUS; rhs = exp; { OpExp {left=lhs; oper=PlusOp; right=rhs} }
    | lhs = exp; MINUS; rhs = exp; { OpExp {left=lhs; oper=MinusOp; right=rhs} }
    | lhs = exp; TIMES; rhs = exp; { OpExp {left=lhs; oper=TimesOp; right=rhs} }
    | lhs = exp; DIV; rhs = exp; { OpExp {left=lhs; oper=DivideOp; right=rhs} }
    | lhs = exp; EQ; rhs = exp; { OpExp {left=lhs; oper=EqOp; right=rhs} }
    | lhs = exp; NE; rhs = exp; { OpExp {left=lhs; oper=NeqOp; right=rhs} }
    | lhs = exp; GE; rhs = exp; { OpExp {left=lhs; oper=GeOp; right=rhs} }
    | lhs = exp; GT; rhs = exp; { OpExp {left=lhs; oper=GtOp; right=rhs} }
    | lhs = exp; LT; rhs = exp; { OpExp {left=lhs; oper=LtOp; right=rhs} }
    | lhs = exp; LE; rhs = exp; { OpExp {left=lhs; oper=LeOp; right=rhs} }
    | lhs = exp; AND; rhs = exp; { IfExp {test=lhs; then'=rhs; else'=Some(IntExp 0)} }
    | lhs = exp; OR; rhs = exp; { IfExp {test=lhs; then'=(IntExp 1); else'=Some(rhs)} }
    | e1 = lvalue; ASSIGN; e2 = exp; { AssignExp {var=e1; exp=e2} }
    | MINUS; e = exp; %prec UMINUS { OpExp {left=(IntExp 0); oper=MinusOp; right=e} }
    | IF; e1 = exp; THEN; e2 = exp; ELSE; e3 = exp; { IfExp {test=e1; then'=e2; else'=Some(e3)} }
    | IF; e1 = exp; THEN; e2 = exp; { IfExp {test=e1; then'=e2; else'=None} }
    | WHILE; e1 = exp; DO; e2 = exp; { WhileExp {test=e1; body=e2} }
    | LPAREN; e = exp; RPAREN; { e }
    | BREAK; { BreakExp }
    | FOR; x = ID; ASSIGN; e1 = exp; TO; e2 = exp; DO; e3 = exp; { ForExp {var=Symbol.symbol x; escape=(ref true); lo=e1; hi=e2; body=e3} }

lvalue:
    | x = ID; { SimpleVar (Symbol.symbol x) }
    | e1 = lvalue; DOT; x = ID; { FieldVar (e1, Symbol.symbol x) }
    | x = ID; LBRACKET; e = exp; RBRACKET; { SubscriptVar (SimpleVar (Symbol.symbol x), e) }
    | e1 = lvalue; LBRACKET; e2 = exp; RBRACKET; { SubscriptVar(e1, e2) }

field:
    | x = ID; EQ; e = exp; { (Symbol.symbol x, e) }

decs:
    | t = list(tydec); v = list(vardec); f = list(fundec); { [TypeDec(t)] @ v @ [FunctionDec(f)] }

tydec:
    | TYPE; x = ID; EQ; t = ty; { {ty_name=(Symbol.symbol x); ty_ty=t} }

ty:
    | x = ID; { NameTy (Symbol.symbol x) }
    | LBRACE; t = separated_list(COMMA, tyfield); RBRACE; { RecordTy t }
    | ARRAY; OF; x = ID; { ArrayTy (Symbol.symbol x) }

tyfield:
    | x = ID; COLON; y = ID; { {field_name=(Symbol.symbol x); field_escape=(ref true); field_ty=(Symbol.symbol y)} }

vardec:
    | VAR; x = ID; ASSIGN; e = exp; { VarDec {var_name=(Symbol.symbol x); var_escape=(ref true); var_ty=None; var_init=e} }
    | VAR; x = ID; COLON; y = ID; ASSIGN; e = exp; { VarDec {var_name=(Symbol.symbol x); var_escape=(ref true); var_ty=Some(Symbol.symbol y); var_init=e} }

fundec:
    | FUNCTION; x = ID; LPAREN; tyfields = separated_list(COMMA, tyfield); RPAREN; EQ; e = exp; { {fun_name=(Symbol.symbol x); fun_params=tyfields; fun_result=None; fun_body=e} }
    | FUNCTION; x = ID; LPAREN; tyfields = separated_list(COMMA, tyfield); RPAREN; COLON; y = ID; EQ; e = exp; { {fun_name=(Symbol.symbol x); fun_params=tyfields; fun_result=Some(Symbol.symbol y); fun_body=e;} }