-- To compile: ghc --make RunShaders .

-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPL-3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Run saved shaders
----------------------------------------------------------------------

module Main where

import System.Environment (getArgs)

import Graphics.Shady.RunSurface (glsl,runShader)

load :: FilePath -> IO ()
load path =
  do (v,f) <- read `fmap` readFile path
     runShader (glsl v f)

main :: IO ()
main =
  do args <- getArgs
     if length args /= 1 then
       putStrLn "I'd like to have an argument, please (file path)"
      else
       load (head args)
