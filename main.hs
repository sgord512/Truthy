module Main where

import Data.List ( nub )
import Data.Maybe ( fromJust )

data Expr = Negation Expr
          | Compound BinaryExpr
          | Statement Letter 
            
data BinaryExpr = Conjunction Expr Expr
                | Disjunction Expr Expr
                | Conditional Expr Expr
                | Biconditional Expr Expr
                  
left :: BinaryExpr -> Expr
left (Conjunction l r) = l
left (Disjunction l r) = l
left (Conditional l r) = l
left (Biconditional l r) = l
right :: BinaryExpr -> Expr                
right (Conjunction l r) = r
right (Disjunction l r) = r
right (Conditional l r) = r
right (Biconditional l r) = r
                
allLetters :: Expr -> [Letter]
allLetters (Negation e) = allLetters e
allLetters (Statement l) = [l]
allLetters (Compound lr) = (allLetters $ left lr) ++ (allLetters $ right lr)

letters :: Expr -> [Letter]
letters = nub $ allLetters 
            
newtype Letter = Letter Char deriving Eq

interpret :: Expr -> [(Letter, Bool)] -> Bool
interpret (Negation e) = (\assignments -> not (interpret e assignments))
interpret (Compound be) = case be of
  Conjunction l r -> (\assignments -> (interpret l assignments) && (interpret r assignments))
  Disjunction l r -> (\assignments -> (interpret l assignments) || (interpret r assignments))
  Conditional l r -> interpret l
  Biconditional l r -> interpret l
interpret (Statement letter) assignments = fromJust $ lookup letter assignments
