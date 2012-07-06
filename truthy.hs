module Main where

import System.Environment
import Text.Parsec
import Truth.Base
import Truth.Parser


main = do (str:_) <- getArgs
          case parse expr "Arguments" str of 
            Left err -> print err
            Right exp -> print exp
            
