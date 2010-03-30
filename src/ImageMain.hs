-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  ImageMain
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GPL-3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- 2D shady example
----------------------------------------------------------------------

module Main where

import Shady.Language.Exp (Pat,R1,R2,FloatE)
import Shady.Language.GLSL (Program,Definition)
import Shady.Color (Color)
import Shady.Image (Image)
import Shady.RunImage (imProg,imProg')

import ImageExamples


-- debugging
p1' :: (Program, Pat R1, Pat R2)
p1' = imProg' anim

-- debugging
p1 :: Definition
p1 = imProg anim


main :: IO ()
main = r anim

anim :: FloatE -> Image Color
anim = a8
