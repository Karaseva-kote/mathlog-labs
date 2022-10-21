module Main where

import Grammar
import Lexer (alexScanTokens)
import Parser
import qualified Data.Map as Map
import Data.Map
import qualified Data.List as List
import Data.List
import qualified Data.Set as Set
import Text.PrettyPrint.Annotated.HughesPJ (Style(mode))

-- -- Axiom
getAxiom expr@(Binary Impl a (Binary Impl b a'))                                                                     | a == a'                                    = AxiomSch "1" expr
getAxiom expr@(Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a' (Binary Impl b' c)) (Binary Impl a'' c'))) | a == a' && a' == a'' && b == b' && c == c' = AxiomSch "2" expr
getAxiom expr@(Binary Impl a (Binary Impl b (Binary And a' b')))                                                     | a == a' && b == b'                         = AxiomSch "3" expr
getAxiom expr@(Binary Impl (Binary And a b) a')                                                                      | a == a'                                    = AxiomSch "4" expr
getAxiom expr@(Binary Impl (Binary And a b) b')                                                                      | b == b'                                    = AxiomSch "5" expr
getAxiom expr@(Binary Impl a (Binary Or a' b))                                                                       | a == a'                                    = AxiomSch "6" expr
getAxiom expr@(Binary Impl b (Binary Or a b'))                                                                       | b == b'                                    = AxiomSch "7" expr
getAxiom expr@(Binary Impl (Binary Impl a c) (Binary Impl (Binary Impl b c') (Binary Impl (Binary Or a' b') c'')))   | a == a' && b == b' && c == c' && c' == c'' = AxiomSch "8" expr
getAxiom expr@(Binary Impl (Binary Impl a b) (Binary Impl (Binary Impl a' (Not b')) (Not a'')))                      | a == a' && a' == a'' && b == b'            = AxiomSch "9" expr
getAxiom expr@(Binary Impl (Not(Not a)) a')                                                                          | a == a'                                    = AxiomSch "10" expr
getAxiom expr@(Binary Impl (Quantifiers Forall x e) e')                                                              | checkFreeOccur x e e'                      = AxiomSch "11" expr
getAxiom expr@(Binary Impl e (Quantifiers Exist x e'))                                                               | checkFreeOccur x e' e                      = AxiomSch "12" expr
getAxiom expr@(Binary Impl (Binary And e0 (Quantifiers Forall x (Binary Impl e ex'))) e')                            | e == e' && checkFreeOccurA9 x e0 e ex'     = AxiomSch "A9" expr
getAxiom expr@(Binary Impl (Equal (Var "a") (Var "b")) (Equal (Inc (Var "a")) (Inc (Var "b"))))                                                                   = Axiom "A1" expr
getAxiom expr@(Binary Impl (Equal (Var "a") (Var "b")) (Binary Impl (Equal (Var "a") (Var "c")) (Equal (Var "b") (Var "c"))))                                     = Axiom "A2" expr
getAxiom expr@(Binary Impl (Equal (Inc (Var "a")) (Inc (Var "b"))) (Equal (Var "a") (Var "b")))                                                                   = Axiom "A3" expr
getAxiom expr@(Not (Equal (Inc (Var "a")) Zero))                                                                                                                  = Axiom "A4" expr
getAxiom expr@(Equal (Sum (Var "a") (Inc (Var "b"))) (Inc (Sum (Var "a") (Var "b"))))                                                                             = Axiom "A5" expr
getAxiom expr@(Equal (Sum (Var "a") Zero) (Var "a"))                                                                                                              = Axiom "A6" expr
getAxiom expr@(Equal (Mul (Var "a") Zero) Zero)                                                                                                                   = Axiom "A7" expr
getAxiom expr@(Equal (Mul (Var "a") (Inc (Var "b"))) (Sum (Mul (Var "a") (Var "b")) (Var "a")))                                                                   = Axiom "A8" expr
getAxiom expr                                                                                                                                                     = ProofNothing

checkFreeOccur x e e' = let r = replace x e e'
                        in Set.null r || ((1 == Set.size r) && (Set.elemAt 0 r /= TermNothing) && not (isTermError (Set.elemAt 0 r)))

haveError set = not (Set.member TermNothing set) && not (Set.null (Set.filter isTermError set))

getError set = Set.elemAt 0 (Set.filter isTermError set)

isTermError term@(TermError v t) = True
isTermError term = False

checkFreeOccurA9 x e0 e ex' = do
                                let r0 = replace x e e0
                                let r' = replace x e ex'
                                1 == Set.size r0 && 1 == Set.size r' && Zero == Set.elemAt 0 r0 && Inc (Var x) == Set.elemAt 0 r'

replace x (Binary Impl a b) (Binary Impl a' b') = Set.union (replace x a a') (replace x b b')
replace x (Binary And a b) (Binary And a' b') = Set.union (replace x a a') (replace x b b')
replace x (Binary Or a b) (Binary Or a' b') = Set.union (replace x a a') (replace x b b')
replace x (Not a) (Not a') = replace x a a'
replace x (PVar a) (PVar a') = if a == a' then Set.empty else Set.singleton TermNothing
replace x (Equal a b) (Equal a' b') = Set.union (replaceTerm x a a') (replaceTerm x b b')
replace x a@(Quantifiers Forall v e) b@(Quantifiers Forall v' e') = replaceQuant x v v' e e' a b
replace x a@(Quantifiers Exist v e) b@(Quantifiers Exist v' e') = replaceQuant x v v' e e' a b
replace x a b = Set.singleton TermNothing

replaceQuant x v v' e e' a b
  | v == x && v == v' && a == b = Set.empty
  | v /= x && v == v' = do
                          let r = replace x e e'
                          if Set.null r then r
                          else
                            if 1 == Set.size r then do
                              let term = Set.elemAt 0 r
                              if term == TermNothing || isTermError term || not (containsInTerm v term) then r
                              else Set.insert (TermError x term) r
                            else r
  | otherwise = Set.singleton TermNothing

containsInExpr v (Binary Impl a b) = containsInExpr v a || containsInExpr v b
containsInExpr v (Binary And a b) = containsInExpr v a || containsInExpr v b
containsInExpr v (Binary Or a b) = containsInExpr v a || containsInExpr v b
containsInExpr v (Equal a b) = containsInTerm v a || containsInTerm v b
containsInExpr v (Not a) = containsInExpr v a
containsInExpr v (PVar a) = False
containsInExpr v (Quantifiers Forall x e) = (v /= x) && containsInExpr v e
containsInExpr v (Quantifiers Exist x e) = (v /= x) && containsInExpr v e

containsInTerm v (Sum a b) = containsInTerm v a || containsInTerm v b
containsInTerm v (Mul a b) = containsInTerm v a || containsInTerm v b
containsInTerm v (Inc a) = containsInTerm v a
containsInTerm v Zero = False
containsInTerm v (Var a) = a == v

replaceTerm x (Sum a b) (Sum a' b') = Set.union (replaceTerm x a a') (replaceTerm x b b')
replaceTerm x (Mul a b) (Mul a' b') = Set.union (replaceTerm x a a') (replaceTerm x b b')
replaceTerm x (Inc a) (Inc a') = replaceTerm x a a'
replaceTerm x Zero Zero = Set.empty
replaceTerm x (Var a) a'
  | a == x = Set.singleton a'
  | Var a == a' = Set.empty
  | otherwise = Set.singleton TermNothing
replaceTerm x a b = Set.singleton TermNothing


getModusPonens :: Expr -> [(Expr, Int)] -> Map Expr Int -> Proof
getModusPonens expr listImlpProof mapProof = do
  let rightEqual = List.filter (\curPair -> getRight (fst curPair) == expr) listImlpProof
  let ans = chooseRight rightEqual mapProof
  if ans /= (-1, -1) then uncurry ModusPonens ans expr
                     else ProofNothing

chooseRight :: [(Expr, Int)] ->  Map Expr Int -> (Int, Int)
chooseRight listRight mapProof = case listRight of
  [] -> (-1, -1)
  x:xs -> let left = Map.findWithDefault (-1) (getLeft (fst x)) mapProof
              next = chooseRight xs mapProof
          in
            if left == (-1) then next
            else
              if (next == (-1, -1)) || ((left, snd x) < next) then (left, snd x) else next

getRule expr@(Binary Impl (Quantifiers Exist x a) b) mapProof | not (containsInExpr x b) && Map.member (Binary Impl a b) mapProof = RuleE (Map.findWithDefault (-1) (Binary Impl a b) mapProof) expr
getRule expr@(Binary Impl a (Quantifiers Forall x b)) mapProof | not (containsInExpr x a) && Map.member (Binary Impl a b) mapProof = RuleA (Map.findWithDefault (-1) (Binary Impl a b) mapProof) expr
getRule expr mapProof = ProofNothing

getErrorProof expr@(Binary Impl (Quantifiers Exist x a) b) mapProof | containsInExpr x b && Map.member (Binary Impl a b) mapProof = Error (": variable " ++ x ++ " occurs free in ?-rule.")
getErrorProof expr@(Binary Impl a (Quantifiers Forall x b)) mapProof | containsInExpr x a && Map.member (Binary Impl a b) mapProof = Error (": variable " ++ x ++ " occurs free in @-rule.")
getErrorProof expr@(Binary Impl e (Quantifiers Exist x e')) mapProof | haveError (replace x e' e) =
  let tErr = getError (replace x e' e)
  in Error (": variable " ++ getV tErr ++ " is not free for term " ++ show (getTerm tErr) ++ " in ?-axiom.")
getErrorProof expr@(Binary Impl (Quantifiers Forall x e) e') mapProof | haveError (replace x e e') =
  let tErr = getError (replace x e e')
  in Error (": variable " ++ getV tErr ++ " is not free for term " ++ show (getTerm tErr) ++ " in @-axiom.")
getErrorProof expr m = Error " is not proved."

getLineProof n proof@(Error s) = "Expression " ++ show n ++ show proof
getLineProof n proof = "[" ++ show n ++ show proof

isProofNothing expr@ProofNothing = True
isProofNothing expr = False

addImpl e@(Binary Impl a b) i list = (e, i) : list
addImpl e i list = list

addToMap x i mapProof 
  | Map.member x mapProof = mapProof
  | otherwise = Map.insert x i mapProof

parseProof :: Int -> [Expr] -> [(Expr, Int)] -> Map Expr Int -> Expr -> [[Char]]
parseProof i list headImplList mapProof provableExpr = case list of
  [] -> []
  x:xs -> let mbAxiom = getAxiom x in
            if isProofNothing mbAxiom then
              let mbModusPonens = getModusPonens x headImplList mapProof in
                if isProofNothing mbModusPonens then
                  let mbRule = getRule x mapProof in
                    if isProofNothing mbRule then
                      [getLineProof i (getErrorProof x mapProof)]
                    else
                      if List.null xs && x /= provableExpr then
                        [getLineProof i mbRule, "The proof proves different expression."]
                      else
                        getLineProof i mbRule : parseProof (i + 1) xs (addImpl x i headImplList) (addToMap x i mapProof) provableExpr
                else
                  if List.null xs && x /= provableExpr then
                    [getLineProof i mbModusPonens, "The proof proves different expression."]
                  else
                    getLineProof i mbModusPonens : parseProof (i + 1) xs (addImpl x i headImplList) (addToMap x i mapProof) provableExpr
            else
              if List.null xs && x /= provableExpr then
                [getLineProof i mbAxiom, "The proof proves different expression."]
              else
                getLineProof i mbAxiom : parseProof (i + 1) xs (addImpl x i headImplList) (addToMap x i mapProof) provableExpr

main :: IO ()
main = do
  input <- getLine
  let problem = parseProblem $ alexScanTokens input
  let provableExpr = get problem
  proof <- getContents
  let listProof = List.map (parseExpr . alexScanTokens) (lines proof)
  let proof = parseProof 1 listProof [] Map.empty provableExpr
  putStrLn (intercalate "\n" (show problem : proof))
