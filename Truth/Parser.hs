module Truth.Parser where

import Text.Parsec
import Text.Parsec.Char ( lower )
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Parsec.Token
import Truth.Base

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
