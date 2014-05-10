{
module Verifier.Parser where

import Automata.DTD
import Data.Functor.Foldable (Fix(..))
import Verifier.Lexer

}

%name parseDTD
%tokentype { Token }
%error { parserError }

%left ','

%token 
  '<'                           { TLeftBracket              }
  '>'                           { TRightBracket             }
  '('                           { TLeftParen                }
  ')'                           { TRightParen               }
  '['                           { TRightSquare              }
  ']'                           { TLeftSquare               }

  '!'                           { TExclam                   }
  ','                           { TComma                    }
  '|'                           { TOption                   }
  '*'                           { TStar                     }
  '+'                           { TPlus                     }
  'DOCTYPE'                     { TDocType                  }
  'PCDATA'                      { TData                     }
  'ELEMENT'                     { TElement                  }

  id                            { TID $$                   }

%%

Document
  : '<' '!' 'DOCTYPE' id '[' Entries ']' '>'
                                    { ADocument $4 $6 }

IDLiteral
  : id                              { $1 }
  | 'ELEMENT'                       { "ELEMENT" }
  | 'DOCTYPE'                       { "DOCTYPE" }

Entries
  : '<' '!' 'ELEMENT' IDLiteral '(' EntryBody ')' '>' Entries
                                    { Fix (AEntry $4 $6) : $9 }
  |                                 { [] }

EntryBody
  : EntryBody ',' EntryBody         {                $1  ++ $3           }
  | id                              { return . Fix $ AReference $1       }
  | id '|' id                       { return . Fix $ AOption $1 $3       }
  | id '*'                          { return . Fix $ ALeastZero $1       }
  | id '+'                          { return . Fix $ ALeastOne  $1       }
  | 'PCDATA'                        { return . Fix $ AData               }

{
parserError :: [Token] -> a
parserError tokens = error $ "Parse error, left over: " ++ concatMap show tokens
}
