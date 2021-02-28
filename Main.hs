module Main where

import Parser
import Text.Parsec
import Typer
import Data.Map

checkAnn s = case parse parseExpr "stdin" s of
            Left err -> print err
            Right t -> case typeSynth empty t of Just _ -> putStrLn "Type checked successfully."
                                                 _      -> putStrLn "Type checking failed."

main = do
    line <- getLine
    checkAnn line
    main