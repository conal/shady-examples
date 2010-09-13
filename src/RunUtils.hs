-- {-# LANGUAGE #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  RunUtils
-- Copyright   :  (c) Conal Elliott 2010
-- License     :  GPL-3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Some utilities for running examples
----------------------------------------------------------------------

module RunUtils where

import Control.Applicative (liftA2)

import Data.Derivative (pureD)

import qualified Shady.Vec as V
import Shady.Language.Type ()  -- Pretty Sample instance
import Shady.Language.Exp
import Shady.Color
import Shady.ParamSurf
import Shady.Lighting
import Shady.Image hiding (rotate2)
-- import Shady.CompileImage (ImageB)
import Shady.CompileSurface (FullSurf) -- SurfB,,surfBProg
-- import Shady.RunSurface (runSurfB)
import Shady.Misc (Action,Sink)
import Shady.MechanicsGL (EyePos)
import Shady.CompileImage (ImageB)


-- For GUIs
import Control.Applicative ((<$>))
import Interface.TV.Gtk
import Data.Lambda
import Shady.RunUI

-- import Graphics.Rendering.OpenGL hiding (Color,rotate,Sink)
-- import qualified Graphics.Glew as Glew




eyePos :: EyePos
-- eyePos = (1,2,2)
-- eyePos = (0,0,-2.5)
eyePos = (1,1,2.5)
-- eyePos = (0,-2,0.001)  -- model disappears when eye_z == 0 and up == (0,-1,0)
-- eyePos = (2,0,0)  -- model disappears when eye_z == 0 and up == (0,-1,0)
-- eyePos = (0,0,-2.5)

eyePosE :: R3E
eyePosE = pureE (V.vec3 ex ey ez) where (ex,ey,ez) = eyePos

fakeTime :: In Float
fakeTime = sliderRIn (0,10) 0

runUI' :: Compile src obj => Out obj -> src -> Action
runUI' = runUI (100,100) eyePos

-- type FullSurf = (Lighter Color, EyePosE -> View, SurfD, Image Color)

surfIm :: HasColor c => SurfD -> Image c -> FullSurf
surfIm surf im = (basicStd, view1, surf, toColor . im)

flatIm :: HasColor c => Image c -> FullSurf
flatIm = surfIm xyPlane
-- flatIm im = (intrinsic, view1, xyPlane , toColor . im)

-- runU :: Sink [SurfB]
-- runU [s] = runUI' animOut (s . pureD)
-- runU _   = error "runU: only single SurfB"

-- I've forgotten what this restriction to single-SurfB is about.
-- Investigate.

animOut :: Out (Sink R1)
animOut = lambda (V.vec1 <$> i) renderOut
 where
   i = -- clockIn
       fakeTime

lambda1 :: In a -> Out b -> Out (One a -> b)
lambda1 i = lambda (V.vec1 <$> i)

type SurfB' = FloatE -> FullSurf

model' :: HasColor c => (T -> SurfD) -> ImageB c -> SurfB'
model' surf im = liftA2 surfIm (surf . pureD) im

-- Animated surface
type SB = T -> SurfD

turning :: SB -> SB
turning f t = onYZ (rotate (0.5 * t)) . f t
