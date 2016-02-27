{-----------------------------------
 - Tests.hs
 - v1.0.1
 -----------------------------------}

module Tests where

import Data.List
import Text.ParserCombinators.Parsec
import qualified Data.HashMap.Strict as H
import Parser
import Interpreter

tests = [
          -- test for quit
          ("quit;", ""),

          -- test for intexp
          ("print 12;", "12"),

          -- test for sequential expression
          ("do print 1; print 2; fi;", "12"),

          -- if/then/else stmts
          ("if true then print 1; else print 0; fi;", "1"),

          -- test for integer operations
          ("print 1+2;", "3"),


          -- TODO: try assignment

        ]