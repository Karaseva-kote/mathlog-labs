module Main where

import Grammar
import Lexer (alexScanTokens)
import Parser
import qualified Data.Map as Map
import Data.Map (Map, member, lookup)
import qualified Data.List as List
import Data.List
import qualified Data.Map as Map
import Grammar (Proof(MyNothing))

getAxiom expr@(Binary Impl a (Binary Impl b a'))                                                                     | a == a'                                    = Axiom 1 a b a expr
getAxiom expr@(Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a' (Binary Impl b' c)) (Binary Impl a'' c'))) | a == a' && a' == a'' && b == b' && c == c' = Axiom 2 a b c expr
getAxiom expr@(Binary Impl a (Binary Impl b (Binary And a' b')))                                                     | a == a' && b == b'                         = Axiom 3 a b a expr
getAxiom expr@(Binary Impl (Binary And a b) a')                                                                      | a == a'                                    = Axiom 4 a b a expr
getAxiom expr@(Binary Impl (Binary And a b) b')                                                                      | b == b'                                    = Axiom 5 a b a expr
getAxiom expr@(Binary Impl a (Binary Or a' b))                                                                       | a == a'                                    = Axiom 6 a b a expr
getAxiom expr@(Binary Impl b (Binary Or a b'))                                                                       | b == b'                                    = Axiom 7 a b a expr
getAxiom expr@(Binary Impl (Binary Impl a c) (Binary Impl (Binary Impl b c') (Binary Impl (Binary Or a' b') c'')))   | a == a' && b == b' && c == c' && c' == c'' = Axiom 8 a b c expr
getAxiom expr@(Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a' (Not b')) (Not a'')))                      | a == a' && a' == a'' && b == b'            = Axiom 9 a b a expr
getAxiom expr@(Binary Impl a (Binary Impl (Not a') b))                                                               | a == a'                                    = Axiom 10 a b a expr
getAxiom expr                                                                                                                                                     = MyNothing 1

getHypothesis :: Expr -> Map Expr Int -> Proof
getHypothesis exp hyp = case Data.Map.lookup exp hyp of
                          Just n  -> Hypothesis n exp
                          Nothing -> MyNothing 1

getModusPonens :: Expr -> [Proof] -> Map Expr Proof -> Proof
getModusPonens expr listImplProof mapProof = do
  let rightEqual = List.filter (\cur -> getRight (getExp cur) == expr) listImplProof
  chooseRight expr rightEqual mapProof

chooseRight :: Expr -> [Proof] -> Map Expr Proof -> Proof
chooseRight expr listRight mapProof = case listRight of
  [] -> MyNothing 1
  x:xs -> let left = Map.findWithDefault (MyNothing 1) (getLeft (getExp x)) mapProof
          in if isNothing left then chooseRight expr xs mapProof
             else ModusPonens x left expr

kindOfProof :: Expr -> [Proof] -> Map Expr Proof -> Map Expr Int -> Int -> Proof
kindOfProof cur headProofImpl headProof hyp numLine =
  let mbAxiom = getAxiom cur
  in 
    if isNothing mbAxiom then
      let mbHypothesis = getHypothesis cur hyp
      in 
        if isNothing mbHypothesis then
          let mbModusPonens = getModusPonens cur headProofImpl headProof
          in 
            if isNothing mbModusPonens then MyNothing numLine
            else mbModusPonens
        else mbHypothesis
    else mbAxiom

isNothing e@(MyNothing a) = True
isNothing e = False

addMap cur pr headProof = if Map.member cur headProof then headProof else Map.insert cur pr headProof

parseProof :: [Proof] -> Map Expr Proof -> Expr -> [Expr] -> Map Expr Int -> Int -> Proof
parseProof headProofImpl headProof cur tailList hyp numLine = case tailList of
  [] -> kindOfProof cur headProofImpl headProof hyp numLine
  x:xs -> let pr = kindOfProof cur headProofImpl headProof hyp numLine
          in if isNothing pr then pr
             else parseProof (getNewHead cur pr headProofImpl) (addMap cur pr headProof) x xs hyp (numLine + 1)

getNewHead exp@(Binary Impl a b) pr headProof = pr : headProof
getNewHead exp pr headProof = headProof

buildAns :: Int -> Proof -> [String] -> [String]
buildAns depth cur hyp = case cur of
  Axiom num a b c e -> getProofAxiom depth num a b c hyp
  Hypothesis num expr -> [addLine depth hyp expr "[Ax]"]
  ModusPonens l r expr -> buildAns (depth + 1) l hyp ++ buildAns (depth + 1) r hyp ++ [addLine depth hyp expr "[E->]"]

getProofAxiom :: Int -> Int -> Expr -> Expr -> Expr -> [String] -> [String]
getProofAxiom depth num a b c hyp = case num of
                                      1 -> proof1 depth a b hyp
                                      2 -> proof2 depth a b c hyp
                                      3 -> proof3 depth a b hyp
                                      4 -> proof4 depth a b hyp
                                      5 -> proof5 depth a b hyp
                                      6 -> proof6 depth a b hyp
                                      7 -> proof7 depth a b hyp
                                      8 -> proof8 depth a b c hyp
                                      9 -> proof9 depth a b hyp
                                      10 -> proof10 depth a b hyp

addLine :: Int -> [String] -> Expr -> String -> String
addLine depth hyp expr act = "[" ++ show depth ++ "] " ++ intercalate "," hyp ++ "|-" ++ show expr ++ " " ++ act

impl2 = Binary Impl

and2 = Binary And

or2 = Binary Or

impl3 a b c = impl2 a (impl2 b c)

proof1 :: Int -> Expr -> Expr -> [String] -> [String]
proof1 depth a b hyp = [addLine (depth + 2) (List.map show [b, a] ++ hyp) a "[Ax]",
                        addLine (depth + 1) (show a : hyp) (impl2 b a) "[I->]",
                        addLine (depth + 0) hyp (impl3 a b a) "[I->]"]

proof2 depth a b c hyp = [addLine (depth + 5) (List.map show [impl2 a b, impl3 a b c, a] ++ hyp) (impl3 a b c) "[Ax]",
                          addLine (depth + 5) (List.map show [impl2 a b, impl3 a b c, a] ++ hyp) a "[Ax]",
                          addLine (depth + 4) (List.map show [impl2 a b, impl3 a b c, a] ++ hyp) (impl2 b c) "[E->]",
                          addLine (depth + 5) (List.map show [impl2 a b, impl3 a b c, a] ++ hyp) (impl2 a b) "[Ax]",
                          addLine (depth + 5) (List.map show [impl2 a b, impl3 a b c, a] ++ hyp) a "[Ax]",
                          addLine (depth + 4) (List.map show [impl2 a b, impl3 a b c, a] ++ hyp) b "[E->]",
                          addLine (depth + 3) (List.map show [impl2 a b, impl3 a b c, a] ++ hyp) c "[E->]",
                          addLine (depth + 2) (List.map show [impl2 a b, impl3 a b c] ++ hyp) (impl2 a c) "[I->]",
                          addLine (depth + 1) (show (impl2 a b) : hyp) (impl2 (impl3 a b c) (impl2 a c)) "[I->]",
                          addLine (depth + 0) hyp (impl3 (impl2 a b) (impl3 a b c) (impl2 a c)) "[I->]"]

proof3 depth a b hyp = [addLine (depth + 3) (List.map show [a, b] ++ hyp) a "[Ax]",
                        addLine (depth + 3) (List.map show [a, b] ++ hyp) b "[Ax]",
                        addLine (depth + 2) (List.map show [a, b] ++ hyp) (and2 a b) "[I&]",
                        addLine (depth + 1) (show a : hyp) (impl2 b (and2 a b)) "[I->]",
                        addLine (depth + 0) hyp (impl3 a b (and2 a b)) "[I->]"]

proof4 depth a b hyp = [addLine (depth + 2) (show (and2 a b) : hyp) (and2 a b) "[Ax]",
                        addLine (depth + 1) (show (and2 a b) : hyp) a "[El&]",
                        addLine (depth + 0) hyp (impl2 (and2 a b) a) "[I->]"]

proof5 depth a b hyp = [addLine (depth + 2) (show (and2 a b) : hyp) (and2 a b) "[Ax]",
                        addLine (depth + 1) (show (and2 a b) : hyp) b "[Er&]",
                        addLine (depth + 0) hyp (impl2 (and2 a b) b) "[I->]"]

proof6 depth a b hyp = [addLine (depth + 2) (show a : hyp) a "[Ax]",
                        addLine (depth + 1) (show a : hyp) (or2 a b) "[Il|]",
                        addLine (depth + 0) hyp (impl2 a (or2 a b)) "[I->]"]

proof7 depth a b hyp = [addLine (depth + 2) (show b : hyp) b "[Ax]",
                        addLine (depth + 1) (show b : hyp) (or2 a b) "[Ir|]",
                        addLine (depth + 0) hyp (impl2 b (or2 a b)) "[I->]"]

proof8 depth a b c hyp = [addLine (depth + 5) (List.map show [impl2 a c, impl2 b c, or2 a b, a] ++ hyp) (impl2 a c) "[Ax]",
                          addLine (depth + 5) (List.map show [impl2 a c, impl2 b c, or2 a b, a] ++ hyp) a "[Ax]",
                          addLine (depth + 4) (List.map show [impl2 a c, impl2 b c, or2 a b, a] ++ hyp) c "[E->]",
                          addLine (depth + 5) (List.map show [impl2 a c, impl2 b c, or2 a b, b] ++ hyp) (impl2 b c) "[Ax]",
                          addLine (depth + 5) (List.map show [impl2 a c, impl2 b c, or2 a b, b] ++ hyp) b "[Ax]",
                          addLine (depth + 4) (List.map show [impl2 a c, impl2 b c, or2 a b, b] ++ hyp) c "[E->]",
                          addLine (depth + 4) (List.map show [impl2 a c, impl2 b c, or2 a b] ++ hyp) (or2 a b) "[Ax]",
                          addLine (depth + 3) (List.map show [impl2 a c, impl2 b c, or2 a b] ++ hyp) c "[E|]",
                          addLine (depth + 2) (List.map show [impl2 a c, impl2 b c] ++ hyp) (impl2 (or2 a b) c) "[I->]",
                          addLine (depth + 1) (show (impl2 a c) : hyp) (impl3 (impl2 b c) (or2 a b) c) "[I->]",
                          addLine (depth + 0) hyp (impl3 (impl2 a c) (impl2 b c) (impl2 (or2 a b) c)) "[I->]"]

proof9 depth a b hyp = [addLine (depth + 5) (List.map show [impl2 a b, impl3 a b Lie, a] ++ hyp) (impl3 a b Lie) "[Ax]",
                        addLine (depth + 5) (List.map show [impl2 a b, impl3 a b Lie, a] ++ hyp) a "[Ax]",
                        addLine (depth + 4) (List.map show [impl2 a b, impl3 a b Lie, a] ++ hyp) (Not b) "[E->]",
                        addLine (depth + 5) (List.map show [impl2 a b, impl3 a b Lie, a] ++ hyp) (impl2 a b) "[Ax]",
                        addLine (depth + 5) (List.map show [impl2 a b, impl3 a b Lie, a] ++ hyp) a "[Ax]",
                        addLine (depth + 4) (List.map show [impl2 a b, impl3 a b Lie, a] ++ hyp) b "[E->]",
                        addLine (depth + 3) (List.map show [impl2 a b, impl3 a b Lie, a] ++ hyp) Lie "[E->]",
                        addLine (depth + 2) (List.map show [impl2 a b, impl3 a b Lie] ++ hyp) (Not a) "[I->]",
                        addLine (depth + 1) (show (impl2 a b) : hyp) (impl3 (impl3 a b Lie) a Lie) "[I->]",
                        addLine (depth + 0) hyp (impl3 (impl2 a b) (impl3 a b Lie) (Not a)) "[I->]"]

proof10 depth a b hyp = [addLine (depth + 4) (List.map show [a, Not a] ++ hyp) (Not a) "[Ax]",
                         addLine (depth + 4) (List.map show [a, Not a] ++ hyp) a "[Ax]",
                         addLine (depth + 3) (List.map show [a, Not a] ++ hyp) Lie "[E->]",
                         addLine (depth + 2) (List.map show [a, Not a] ++ hyp) b "[E_|_]",
                         addLine (depth + 1) (show a : hyp) (impl2 (Not a) b) "[I->]",
                         addLine (depth + 0) hyp (impl3 a (Not a) b) "[I->]"]

main :: IO ()
main = do
  input <- getLine
  let problem = parseProblem $ alexScanTokens input
  let hypothesis = Map.fromList (zip (getContext problem) [0..])
  let listHypo = List.map show (getContext problem)
  let provableExpr = getExpr problem
  proof <- getContents
  let listProof = List.map (parseExpr . alexScanTokens) (lines proof)
  if (listProof == []) || (last listProof /= provableExpr) then
    putStrLn "The proof does not prove the required expression"
  else do
    let proof = parseProof [] Map.empty (head listProof) (tail listProof) hypothesis 2
    if isNothing proof then 
      putStrLn ("Proof is incorrect at line " ++ show (getNum proof))
    else do
      let tree = buildAns 0 proof listHypo
      putStrLn (intercalate "\n" tree)
