module Truth.Base where

import Data.List ( nub )
import Data.Maybe ( fromJust )
import qualified Util.Unicode as U

data Expr = Negation Expr
          | Compound BinaryExpr
          | Statement Letter 
            
instance Show Expr where
  show (Negation e) = "~" ++ show e
  show (Compound be) = "(" ++ show be ++ ")"
  show (Statement l) = show l

data BinaryExpr = Conjunction Expr Expr
                | Disjunction Expr Expr
                | Conditional Expr Expr
                | Biconditional Expr Expr
                  
instance Show BinaryExpr where
  show (Conjunction l r) = show l ++ U.c2s U.and ++ show r
  show (Disjunction l r) = show l ++ U.c2s U.or ++ show r
  show (Conditional l r) = show l ++ U.c2s U.conditional ++ show r
  show (Biconditional l r) = show l ++ U.biconditional:" " ++ show r
                  
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
letters = nub . allLetters 
            
newtype Letter = Letter Char deriving Eq

instance Show Letter where 
  show (Letter c) = U.c2s c 

type Assignment = (Letter, Bool)
type Interpretation = [Assignment]

interpret :: Expr -> Interpretation -> Bool
interpret (Negation e) assignments = not $ interpret e assignments
interpret (Compound be) assignments = case be of
  Conjunction l r -> interpret l assignments && interpret r assignments
  Disjunction l r -> interpret l assignments || interpret r assignments
  Conditional l r -> interpret l assignments
  Biconditional l r -> interpret l assignments
interpret (Statement letter) assignments = fromJust $ lookup letter assignments

allInterpretations :: [Letter] -> [Interpretation]
allInterpretations [] = [[]]
allInterpretations (l:letters) = concatMap (addBoth l) (allInterpretations letters)
 where addBoth = \l interp -> [(l, True):interp, (l, False):interp]

truthTable :: Expr -> [(Interpretation, Bool)]              
truthTable e = let interpretations = (allInterpretations $ allLetters e)
               in zip interpretations (map (interpret e) interpretations)
                  

a = Statement $ Letter 'a'
b = Statement $ Letter 'b'
c = Compound $ Conjunction a b
