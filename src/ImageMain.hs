-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  ImageMain
-- Copyright   :  (c) Conal Elliott 2009-2010
-- License     :  GNU GPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- 2D shady example
----------------------------------------------------------------------

module Main where

import Shady.Language.Exp (R1,R2,FloatE)
import Shady.Color (Color)
import Shady.Image (Image)
import Shady.CompileE (GLSL)
import Shady.CompileImage (imageBProg)

import ImageExamples

-- -- debugging
-- p1' :: (Program, Pat R1, Pat R2)
-- p1' = imProg' anim

-- debugging
p1 :: GLSL R1 R2
p1 = imageBProg anim


main :: IO ()
main = run anim

anim :: FloatE -> Image Color
anim = a8
