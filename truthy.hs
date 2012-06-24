module Truthy where

import Data.List ( nub )
import Data.Maybe ( fromJust )
import Text.Parsec
import Text.Parsec.Char ( lower )
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Parsec.Token

or = "|"
and = "&"

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
  show (Conjunction l r) = show l ++ "&" ++ show r
  show (Disjunction l r) = show l ++ "|" ++ show r
  show (Conditional l r) = show l ++ "->" ++ show r
  show (Biconditional l r) = show l  ++ "<->" ++ show r
                  
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
  show (Letter c) = show c

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

addBothInterpretations ::  [Interpretation] -> Letter -> [Interpretation]
addBothInterpretations interpretations l = concatMap (\interpretation -> [(l, True):interpretation,(l, False):interpretation]) interpretations

allInterpretations :: [Letter] -> [Interpretation]
allInterpretations letters = foldl addBothInterpretations [[]] letters


truthTable :: Expr -> [(Interpretation, Bool)]              
truthTable e = let interpretations = (allInterpretations $ allLetters e)
               in zip interpretations (map (interpret e) interpretations)
                  
expr :: Parser Expr
expr = buildExpressionParser table term
             <?> "expression"

term :: Parser Expr
term = do { char '('
          ; e <- expr
          ; char ')'
          ; return e
          }
       <|> variable
       <?> "simple expression"

-- table :: OperatorTable Char () Expr
table = [ [prefix "~" (\e -> Negation e)]
        , [binary "&" (\l r -> Compound $ Conjunction l r) AssocLeft, binary "|" (\l r -> Compound $ Disjunction l r) AssocLeft]
        , [binary "->" (\l r -> Compound $ Conditional l r) AssocLeft, binary "<->" (\l r -> Compound $ Biconditional l r) AssocLeft]
        ]
           
binary str constructor associativity = Infix (do { try $ string str
                                                 ; return constructor 
                                                 }) associativity
                                       
prefix str constructor = Prefix (do { try $ string str
                                    ; return constructor })
           
variable :: Parser Expr
variable = do { var <- lower           
              ; return (Statement $ Letter var)
              }

a = Statement $ Letter 'a'
b = Statement $ Letter 'b'
c = Compound $ Conjunction a b
