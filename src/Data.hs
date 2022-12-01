module Data
    ( grid
    , languages
    ) where

import Grid (Grid)

grid :: Grid Char
grid = [ "__C________R_R_"
       , "__SI________U__"
       , "__HASKELL__S_B_"
       , "__A__A____TS__Y"
       , "__R___B___C____"
       , "__PHP____H_____"
       , "____S_LREP_____"
       , "____I__M_Y__L__"
       , "____L_E__T_O___"
       , "_________HB____"
       , "_________O_____"
       , "________CN_____"
       ]

languages :: [String]
languages = [ "BASIC"
            , "COBOL"
            , "CSHARP"
            , "HASKELL"
            , "LISP"
            , "PERL"
            , "PHP"
            , "PYTHON"
            , "RUBY"
            , "SCHEME"
            , "RUST"
            ]
