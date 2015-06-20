module Main where

import ProjectEuler.Problem001
import ProjectEuler.Problem002
import ProjectEuler.Problem003
import ProjectEuler.Problem004
import ProjectEuler.Problem005
import ProjectEuler.Problem006
import ProjectEuler.Problem007
import ProjectEuler.Problem008
import ProjectEuler.Problem009
import ProjectEuler.Problem010


import Util
import qualified Data.Map as M
import System.Environment (getArgs)
import System.Exit        (exitSuccess)

solutions :: M.Map Int String
solutions = M.fromList [
      (1,show solution001),
      (2,show solution002),
      (3,show solution003),
      (4,show solution004),
      (5,show solution005),
      (6,show solution006),
      (7,show solution007),
      (8,show solution008),
      (9,show solution009),
      (10,show solution010)]









main :: IO ()
main = do
      args <- getArgs
      case args of
         [number] -> case M.lookup (read number :: Int) solutions of
                        Just result -> time result >>= print
                        Nothing     -> putStrLn("No solutions found.") >> exitSuccess
         _        -> putStrLn("Help? cabal run [number]") >> exitSuccess
