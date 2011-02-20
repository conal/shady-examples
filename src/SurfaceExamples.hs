{-# LANGUAGE ScopedTypeVariables, TypeOperators, FlexibleContexts
           , TypeFamilies
  #-}
{-# OPTIONS_GHC -Wall
                -fno-warn-missing-signatures
                -fno-warn-type-defaults
                -fno-warn-unused-imports
  #-}
----------------------------------------------------------------------
-- |
-- Module      :  SurfaceExamples
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  GNU GPLv3
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- 3D shady examples
----------------------------------------------------------------------

module SurfaceExamples where

import Control.Applicative ((<*>),pure,liftA2)

import Control.Compose ((~>),result)

import Data.VectorSpace
import Data.Derivative (pureD,powVal)

import Text.PrettyPrint.Leijen (Doc,pretty)

import Data.Boolean
import qualified Shady.Vec as V
import Text.PrettyPrint.Leijen.DocExpr (HasExpr)
import Shady.Complex
import Shady.Language.Type ()  -- Pretty Sample instance
import Shady.Language.Exp
import Shady.Color
import Shady.Image hiding (rotate2)
import Shady.ITransform (ITrans)
import qualified Shady.Image as I
import Shady.ParamSurf
import Shady.Lighting
import Shady.CompileImage (ImageB)
import Shady.CompileSurface (SurfB,FullSurf,surfBProg)
import Shady.RunSurface (runSurfB)
import Shady.Misc (Unop,Binop,Action,Sink,frac,clamp)
import Shady.MechanicsGL (EyePos)

import ImageExamples (samplerIm')
import qualified ImageExamples as TI

-- For GUIs
import Control.Applicative ((<$>))
import Control.Compose (cofmap)
import Interface.TV.Gtk
import Data.Lambda
import Data.Pair
import Data.Title
import Shady.RunUI

import Graphics.UI.Gtk as Gtk hiding (Color,Action,Image,Point)
import Graphics.UI.Gtk.OpenGL as GtkGL
import Graphics.Rendering.OpenGL hiding (Color,rotate,Sink)
-- import qualified Graphics.Glew as Glew

import RunUtils
import ImageExamples (samplerIn')


-- Pretty-print for debugging
p :: SurfB -> Doc
p = pretty . surfBProg eyePosE

model :: forall c. HasColor c =>
         (T -> SurfD) -> ImageB c
      -> SurfB
model surf im t' = (l, view1,surf t', toColor . im (powVal t'))
 where
   l = -- ambDiff (0.5,0.5)
       -- intrinsic
       basicStd
       -- basicRV (ka,kd,ks,ksh)
       -- basicNH (gray 0.2,gray 0.4,gray 0.5,ksh)
       -- basicRV (gray 0.2,gray 0.4,gray 0.5,ksh)
       -- basicRV (gray 0.0,gray 0.4,gray 0.5,ksh)
       -- basicNH (ka,kd,ks,ksh)
       -- basicRV (0.2,0.4,ks,ksh)
       -- diffuse
       -- intrinsic * diffuse
       -- ambDiff (ka,kd)
       -- specularRV ksh
       -- specularNH ksh

ka,kd,ks :: Color
ksh :: FloatE

ka = gray 0.3                          -- ambient
kd = gray 0.6                          -- diffuse
ks = gray 0.5                          -- specular
ksh = 15                               -- specular exponent


-- With a 3D texture
simple3 :: SurfD -> FullSurf
simple3 surf = (lighter,view,surf,img)
 where
   lighter = basicStd
   view    = view1
   img     = toColor . img3 . surf . p2T2
   img3    = checker3 . (2 *^)
   p2T2 (u :+ v) = pureD u :+ pureD v


checker3 :: V3 T -> BoolE
checker3 (a,b,c) = (f a /=* f b) /=* f c
  where f = (>* 0.5) . frac . powVal

-- TODO: find a better 3D texture.  3D checker is visually confusing.



a0 :: SB
a0 _ = hfSurf 0

a1 :: SB
a1 _ = hfSurf (\ (u :+ v) -> 0.5 * (sin (4*u) + sin (3*v)))

a1b :: SB
a1b t = hfSurf (\ (u :+ v) -> 0.1 * (sin (6*u* cos (1.1*t)) + cos (7 * v * sin (0.9*t))))

a1d :: SB
a1d t = hfSurf (\ q -> sin (2*t + 7 * magnitude q) / 3)

a1e :: SB
a1e t = hfSurf (\ q -> sin (2*t + 9 * magnitude q) / (1 + magnitude q)**3)

a2 :: SB
a2 = const sphere1

a3 :: SB
a3 t = ((1 + sin t / 3) *^) . sphere1

a3b :: SB
a3b t = ((a,b,0) ^+^) . sphere1
 where
   a :+ b = cis t

a4 :: SB
a4 = const (torus (3/4) (1/3))

-- | Conditional based on function output
condO :: IfB bool b => (b -> bool) -> Binop (a -> b)
condO h f g = ifB (h . f) f g


{--------------------------------------------------------------------
    Displacement etc
--------------------------------------------------------------------}

crate :: SB
crate t = hfSurf (0.5 * (eggcrateH . rotate t . (2 *^)))

-- 1D bump.

-- This stuff looks generally terrible.  It has derivative discontinuities
-- and is give piecewise-linear approximation that is insensitive to
-- non-linearities.  TODO: figure out how to tessellate adaptively.  Could
-- perhaps combine derivatives and interval analysis.  Want to perform
-- dynamically, on the GPU.  OpenGL standards don't yet support.  Might
-- have to wait for switch to OpenCL.

-- bump1 :: (Ord s, Floating s) => Warp1 s
-- bump1 = -- sin . acos
--         sin . acos . clamp (-1,1)

bump1 :: (IfB bool s, OrdB bool s, Floating s) => Warp1 s
bump1 = -- sin . acos
        ifB (inRange (-1,1)) (sin . acos) 0

inRange :: (OrdB bool t) => (t, t) -> t -> bool
inRange (lo,hi) x = lo <=* x &&* x <=* hi

-- 2D bump as height field
bumpH :: ( IfB bool s, OrdB bool s
         , Floating s, InnerSpace s, s ~ Scalar s) => HeightField s
bumpH = bump1 . magnitude

bump :: ( IfB bool s, OrdB bool s
        , Floating s, InnerSpace s, s ~ Scalar s) => Surf s
bump = hfSurf bumpH


-- utile :: (Frac p, ITrans (Complex s) p, ITrans (Complex s) a, Floating s) => 
--          Unop (p -> a)

-- Scaled down bump.  2 gives abutting hemispheres.  >2 gives space.
bumpH' = ((/ 3) . bumpH . (3 *^))

bump' :: SurfD
bump' = hfSurf bumpH'

bumpsH :: HeightField T
bumpsH = utile bumpH'

bumps :: SurfD
bumps = hfSurf bumpsH
        -- utile bump'

-- In this version, the bumps all coincide.  Hm.  What does it mean to
-- tile a surface?
bumps' = utile (hfSurf bumpH')

bumps'' :: SurfD
bumps'' = ((/ 2) . bumps . (2 *^))


blackHoleH :: Floating s => HeightField s
blackHoleH = recip . magnitude

blackHole :: Floating s => Surf s
blackHole = hfSurf blackHoleH

blackHole' :: Floating s => Surf s
blackHole' = profile circle (fcurve recip)

bumpySphere = displace sphere1 (utile bumpH')


{--------------------------------------------------------------------
    Run
--------------------------------------------------------------------}

-- run :: Sink [SurfB]
-- run = runSurfB eyePos

-- run = runSurfB' (,n,) eyePos where n = 100    -- stange!  over 105 and i lose much of the shape


{--------------------------------------------------------------------
    Demos
--------------------------------------------------------------------}

m surf im = [model surf im]

m' surf im = m (turning surf) im

mm surf im = (m surf im, m' surf im)

-- 'run' these demos

tt1@(t1,t1')    = mm a1e TI.a4
tt2@(t2,t2')    = mm a2 TI.a4
tt3@(t3,t3')    = mm a1e TI.a19
tt4@(t4,t4')    = mm a1e  TI.a19b
tt5@(t5,t5')    = mm a1e  TI.a21a
tt6@(t6,t6')    = mm a1e  TI.a21c
tt7@(t7,t7')    = mm (const  bump) TI.a22b
tt8@(t8,t8')    = mm (const bump) TI.a22b
tt9c@(t9c,t9c') = mm (const bumps) TI.a4
tta@(ta,ta')    = mm (utile . a1e)  TI.a4 -- OMG weird
ttc@(tc,tc')    = mm a4  (const checker)
tte@(te,te')    = mm a4  (\t -> translate2 (cos t :+ 0) $ scale2 (1/2 :+ 1/5) checker)
ttf@(tf,tf')    = mm a4  (\t -> translate2 (cos (t/2) :+ sin (t/5)) $ scale2 (1/2 :+ 1/5) checker)
tth@(th,th')    = mm (const blackHole')  TI.a8

ti  = tf  ++ th
ti' = tf' ++ th'
tti = (ti,ti')

ttj@(tj,tj') = mm (const bumpySphere) TI.a22b
ttk@(tk,tk') = mm (const  bumps'') TI.a4
ttl@(tl,tl') = mm a2  (const $ I.scale2 (1/3 :+ 1/4) I.checker)


-- Flat examples.  (Use interpolated normals, since the normal is constant.)
qq imb = mm a0 imb

qq2@(q2,q2')       = qq TI.a2
qq3@(q3,q3')       = qq TI.a3
qq4@(q4,q4')       = qq TI.a4
qq8@(q8,q8')       = qq TI.a8
qq9b@(q9b,q9b')    = qq TI.a9b
qq17@(q17,q17')    = qq TI.a17
qq19@(q19,q19')    = qq TI.a19
qq19b@(q19b,q19b') = qq TI.a19b
qq21a@(q21a,q21a') = qq TI.a21a
qq21c@(q21c,q21c') = qq TI.a21c
qq22b@(q22b,q22b') = qq TI.a22b


-- or print the program.
ph :: [SurfB] -> Doc
ph [s] = p s
ph _   = error "ph: only handles one model"

save :: String -> ([SurfB],[SurfB]) -> IO ()
save name ([s],[s']) = do saveSurf  name           s
                          saveSurf (name++"-anim") s'
save name  _    = error $ "save: " ++ name ++ " only handles one model"

saveSurf name s = putStrLn name >>
                  TI.saveSh name (surfBProg eyePosE s)

saveAll = do save "t1"   tt1
             save "t3"   tt3
             save "t4"   tt4
             save "t5"   tt5
             save "t6"   tt6
             save "ta"   tta
             save "tc"   ttc
             save "te"   tte
             save "tf"   ttf
             save "th"   tth
             save "tl"   ttl

             save "q2"   qq2
             save "q3"   qq3
             save "q4"   qq4
             save "q8"   qq8
             save "q9b"  qq9b
             save "q17"  qq17
             save "q19"  qq19
             save "q19b" qq19b
             save "q21q" qq21a
             save "q21c" qq21c
             save "q22b" qq22b

-- Some faves: t1', t4', tf', t6',ta'

q1 :: T -> T -> SB
q1 a b t = hfSurf (\ q -> sin (a*t + b * magnitude q) / (1 + magnitude q)**3)

tq1 = runUI' (h "rate" 2 $ h "waviness" 10 $ lambda1 clockIn $ renderOut)
         (\ a b -> model' (turning $ q1 (pureD a) (pureD b)) TI.a4)
 where
   h tit x0 = title tit $ lambda1 (sliderRIn (0,20) x0)

-- model' surf im t = surfIm (surf (pureD t)) (im t)

tq2 = runUI' out $ \ sam t -> surfIm (turning a1e (pureD t)) (samplerIm' sam)
 where
   out = title "Wavy picture (simple)" $ lambda samplerIn' $ lambda1 clockIn renderOut

tq4 = runUI' out $
          \ sam sr cr lim t ->
              surfIm
                (turning (const $ torus (pureD sr) (pureD cr)) (pureD t))
                (orbit2 lim t (samplerIm' sam))
 where
   out = title "Textured torus"
         lambda samplerIn' $ h "sweep" (3/4) $ h "cross" (1/3) $
         shiftUI $
         lambda1 clockIn $ renderOut
   h str x0 = lambda1 (title (str ++ " radius") $ sliderRIn (0,1) x0)

-- Orbit an image around the origin, at a distance of lim.
orbit2 :: ER -> ER -> Filter Color
orbit2 lim = translate2 . (lim *^) . cis . (/ 2)

shiftUI = lambda1 (title "texture shift" $ sliderRIn (0,2) (1/2))


-- tq3' = runUI' (twice out) $
--           \ sam a b lim t ->
--               twice $ surfIm
--                 (turning (q1 (pureD a) (pureD b)) (pureD t))
--                 (orbit2 lim t (samplerIm' sam))
--  where
--    out = lambda samplerIn' $ h 2 $ h 10 $ shiftUI $ lambda1 clockIn $ renderOut
--    h :: Float -> Out b -> Out (R1 -> b)
--    h x0 = lambda1 (sliderRIn (0,20) x0)

-- twice z = pair z z


q5 :: T -> T -> Surf T
q5 w t = hfSurf (\ q -> sin (t + w * magnitude q) / (1 + magnitude q)**3)

tq5 = runUI' out $
          \ sam w lim t ->
              surfIm
                (onYZ (rotate (0.5 * pureD t)) . q5 (pureD w) (pureD t))
                (orbit2 lim t (samplerIm' sam))
 where
   out = title "Wavy picture" $
         lambda samplerIn' $ waveO 10 $ shiftUI $ rateO 1 $ renderOut

waveO :: Float -> Out b -> Out (R1 -> b)
waveO x0 = lambda1 (title "waviness" $ sliderRIn (0,20) x0)

rateO :: Float -> Out b -> Out (R1 -> b)
rateO x0 = lambda1 (title "rate" $ rateSliderIn (-10,10) x0)


runImUI :: Compile src obj => Out obj -> src -> Action
runImUI = runUI (2,2) (0,0,2.01)

tq6 = runImUI out $ (result.result.result) flatIm im
 where
   im a b s = lerp (samplerIm' a) (samplerIm' b) (pure s)
   out = title "Image interpolation" $
         lambda samplerIn' $ lambda samplerIn' $
         lambda1 i $ renderOut
   i = title "fraction" $ sliderRIn (0,1) 0.5

tq7 = runImUI out $ (result.result.result) flatIm im
 where
   im a b s = boolean (samplerIm' a) (samplerIm' b) (disk (sinU s))
   out = title "disk choice (animated)" $
         lambda samplerIn' $ lambda samplerIn' $
         lambda1 (title "rate" $ rateSliderIn (-5,5) 0.5) $ renderOut


type Fraction  = R1
type FractionE = FloatE

invertingF :: FractionE -> (FloatE -> FloatE)
invertingF w = (** (1 / lerp 1 (-1) w))

-- | Use @f@ to move points from current distance-from-origin to new one.
radialTrans :: Unop FloatE -> Filter c
radialTrans f = uscale2Im (scaleFac . magnitude)
 where
    scaleFac d = d / f d

inverting :: FractionE -> Filter c
inverting = radialTrans . invertingF

-- invertingUI :: Fraction -> Float -> UITrans
-- invertingUI w inv = transformUIFromUScale "inversion" inv $
--                     liftM inverting $
--                     slider (0,1)
--                       "inside-outness"
--                       "how far from normal to completely inverted" w

invertIn x0 = vec1 <$> title "progress" (sliderRIn (0,1) x0)

invertO x0 = lambda (invertIn x0)

tq9 = runImUI out $ result flatIm (flip inverting checker)
 where
   out = title "checker inversion"
         invertO (1/4) renderOut

tq10 = runUI' out $ \ w inv t ->
          surfIm (turning (q5 (pureD w)) (pureD t)) (inverting inv checker)
 where
   out = title "Wavy checker inversion"
         waveO 10 $ invertO (1/4) $ rateO 1 $ renderOut

main = tq10
