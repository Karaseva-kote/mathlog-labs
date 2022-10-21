{
module Lexer where
}

%wrapper "basic"

$upAlpha = [A-Z]
$lowAlpha = [a-z]

tokens :-

  $white+                             ;
  "#"                                 ;
  \.                                  { \_ -> Dot }
  @                                   { \_ -> At }
  \?                                  { \_ -> Question }
  "|-"                                { \_ -> Turn }
  \(                                  { \_ -> LeftP }
  \)                                  { \_ -> RightP }
  \|                                  { \_ -> OrT }
  &                                   { \_ -> AndT }
  "->"                                { \_ -> ImplT }
  !                                   { \_ -> NotT }
  $upAlpha                            { \s -> Up s }
  $lowAlpha                           { \s -> Low s }
  =                                   { \_ -> Eq }
  \+                                  { \_ -> Plus }
  \*                                  { \_ -> Multi }
  \'                                  { \_ -> Apost }
  0                                   { \_ -> ZeroT }

{

data Token = Dot
           | At
           | Question
           | AndT
           | OrT
           | ImplT
           | NotT
           | Turn
           | LeftP
           | RightP
           | Up String
           | Low String
           | Eq
           | Plus
           | Multi
           | Apost
           | ZeroT
           deriving (Show, Eq)

}