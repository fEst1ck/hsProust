module Lexer( hsProustDef, lexer ) where
import Text.Parsec ( alphaNum, oneOf, (<|>) )
import Text.Parsec.Language (haskellStyle, LanguageDef)
import Text.Parsec.Token
hsProustDef :: LanguageDef st
hsProustDef = haskellStyle
            {
                identLetter = alphaNum <|> oneOf "_'",
                opLetter = opLetter haskellStyle <|> oneOf "∨∧¬→⊥",
                reservedOpNames = ["\\/", "∨",
                                   "/\\", "∧",
                                   "~", "¬",
                                   "->", "→",
                                   "⊥",
                                   ":",
                                   "\\", "=>"]
            }

lexer = makeTokenParser hsProustDef