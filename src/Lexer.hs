module Lexer( hsProustDef, lexer ) where
import Text.Parsec ( alphaNum, oneOf, (<|>) )
import Text.Parsec.Language (haskellStyle, LanguageDef)
import Text.Parsec.Token
import Text.Parsec.Char
hsProustDef :: LanguageDef st
hsProustDef = haskellStyle
            {
                identStart = identStart haskellStyle <|> char '_',
                identLetter = alphaNum <|> oneOf "_'",
                opLetter = opLetter haskellStyle <|> oneOf "∨∧¬→⊥λ",
                reservedOpNames = ["\\/", "∨",
                                   "/\\", "∧",
                                   "~", "¬",
                                   "->", "→",
                                   "⊥",
                                   ":",
                                   "\\", "λ", "=>"]
            }

lexer = makeTokenParser hsProustDef
