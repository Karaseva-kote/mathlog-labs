{
module Parser where

import Grammar
import Lexer
}

%name      parseProblem Problem
%name      parseExpr Expr
%tokentype { Token }
%error     { parseError }


%token IDENT  { Ident $$ }
%token IMPL   { ImplT }
%token OR     { OrT }
%token AND    { AndT }
%token NOT    { NotT }
%token LEFTP  { LeftP }
%token RIGHTP { RightP }
%token COMMA  { Comma }
%token TURN   { Turn }

%%

Problem
  : Context TURN Expr  { Problem $1 $3 }

Context
  : Expr               { $1 : [] }
  | Expr COMMA Context { $1 : $3 }
  | {- empty -}        { [] }

Expr
  : Disj               { $1 }
  | Disj IMPL Expr     { Binary Impl $1 $3 }

Disj
  : Conj               { $1 }
  | Disj OR Conj       { Binary Or $1 $3 }

Conj
  : Term               { $1 }
  | Conj AND Term      { Binary And $1 $3 }

Term
  : NOT Term           { Not $2 }
  | LEFTP Expr RIGHTP  { $2 }
  | IDENT              { Var $1 }

{
parseError e = error "parseError"
}
