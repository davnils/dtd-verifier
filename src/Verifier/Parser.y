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
%left '|'

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
  : '<' '!' 'ELEMENT' IDLiteral '(' EntryBodyList ')' '>' Entries
                                    { Fix (AEntry $4 $6) : $9 }
  |                                 { [] }

EntryBodyList
  : EntryBody ',' EntryBodyList     { $1 : $3           }
  | EntryBody                       { [$1]              }

ConcreteEntry
  : id                              { Fix $ AReference $1       }
  | '(' EntryBody ')'               { $2       }

EntryBody
  : id                              { Fix $ AReference $1       }
  | ConcreteEntry '|' EntryBody     { Fix $ AOption    $1 $3    }
  | ConcreteEntry '*'               { Fix $ ALeastZero $1       }
  | ConcreteEntry '+'               { Fix $ ALeastOne  $1       }
  | 'PCDATA'                        { Fix $ AData               }

{
parserError :: [Token] -> a
parserError tokens = error $ "Parse error, left over: " ++ concatMap show tokens
}
