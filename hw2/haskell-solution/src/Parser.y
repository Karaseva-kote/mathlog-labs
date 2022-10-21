{
module Parser where

import Grammar
import Lexer
}

%name      parseProblem Problem
%name      parseExpr Expr
%tokentype { Token }
%error     { parseError }


%token UP      { Up $$ }
%token LOW     { Low $$ }
%token IMPL    { ImplT }
%token OR      { OrT }
%token AND     { AndT }
%token NOT     { NotT }
%token LEFTP   { LeftP }
%token RIGHTP  { RightP }
%token TURN    { Turn }
%token DOT     { Dot }
%token FORALL  { At }
%token EXIST   { Question }
%token EQ      { Eq }
%token PLUS    { Plus }
%token MUL     { Multi }
%token INC     { Apost }
%token ZERO    { ZeroT }

%%

Problem
  : TURN Expr  { Problem $2 }

Expr
  : Disj               { $1 }
  | Disj IMPL Expr     { Binary Impl $1 $3 }

Disj
  : Conj               { $1 }
  | Disj OR Conj       { Binary Or $1 $3 }

Conj
  : Unary               { $1 }
  | Conj AND Unary      { Binary And $1 $3 }

Unary
  : Predicate           { $1 }
  | NOT Unary           { Not $2 }
  | LEFTP Expr RIGHTP   { $2 }
  | FORALL LOW DOT Expr { Quantifiers Forall $2 $4 }
  | EXIST LOW DOT Expr  { Quantifiers Exist $2 $4 }

Predicate
  : UP           { PVar $1 }
  | Term EQ Term { Equal $1 $3 }

Term
  : Summand           { $1 }
  | Term PLUS Summand { Sum $1 $3 }

Summand
  : Multiplied             { $1 }
  | Summand MUL Multiplied { Mul $1 $3 }

Multiplied
  : LOW               { Var $1 }
  | LEFTP Term RIGHTP { $2 }
  | ZERO              { Zero }
  | Multiplied INC    { Inc $1 }

{
parseError e = error "parseError"
}
