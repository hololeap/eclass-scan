module Main (main) where

import Distribution.Gentoo.Eclass
import Text.Pretty.Simple

main :: IO ()
main = scanRepos >>= pPrint

