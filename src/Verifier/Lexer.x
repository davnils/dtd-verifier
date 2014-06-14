{
module Verifier.Lexer where

import Data.Char (isAlpha)

}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white                        ;
  "<"                           {tag' TLeftBracket              }
  ">"                           {tag' TRightBracket             }
  "("                           {tag' TLeftParen                }
  ")"                           {tag' TRightParen               }
  "["                           {tag' TRightSquare              }
  "]"                           {tag' TLeftSquare               }

  "!"                           {tag' TExclam                   }
  ","                           {tag' TComma                    }
  "|"                           {tag' TOption                   }
  "*"                           {tag' TStar                     }
  "+"                           {tag' TPlus                     }
  "?"                           {tag' TOptional                 }
  "DOCTYPE"                     {tag' TDocType                  }
  "\#PCDATA"                    {tag' TData                     }
  "ELEMENT"                     {tag' TElement                  }

  $alpha+                       {tag $ TID                      }

{

data Token
 = TLeftBracket
 | TRightBracket
 | TLeftParen
 | TRightParen
 | TRightSquare
 | TLeftSquare
   
 | TExclam
 | TComma
 | TOption
 | TStar
 | TPlus
 | TOptional
 | TDocType
 | TData
 | TElement

 | TID String
 deriving (Eq, Ord, Show)

type SourceInfo = (Int, Int)

tag :: (String -> Token) -> AlexPosn -> String -> (Token, SourceInfo)
tag f (AlexPn _ row col) input =  (f input, (row, col))

tag' :: Token -> AlexPosn -> String -> (Token, SourceInfo)
tag' res pos =  tag (const res) pos

}
