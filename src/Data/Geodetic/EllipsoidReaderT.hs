{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Geodetic.EllipsoidReaderT(
  EllipsoidReaderT(..)
, EllipsoidReader
, runEllipsoidReader
, toEllipsoidReaderT
, hoistEllipsoidReader
, arrEllipsoidReader
, readEllipsoid
, readSemiMajor
, readFlattening
, readFlatteningReciprocal
, semiMinor
, eccentricitySquared
, eccentricitySquared'
, distributeNormal
, normal
, wgs84'
, wgs84''
, earthGeo
) where

import Control.Applicative(Alternative((<|>), empty), liftA2)
import qualified Control.Monad as Monad(return, (>>=))
import Control.Monad(MonadPlus(mzero, mplus))
import Control.Monad.Fix(MonadFix(mfix))
import Control.Monad.IO.Class(MonadIO(liftIO))
import Control.Monad.Trans.Class(MonadTrans(lift))
import Control.Monad.Trans.Reader(ReaderT)
import Control.Monad.Zip(MonadZip(mzip))
import Control.Lens (makeWrapped, Iso', ReifiedIso', (^.), iso, ReifiedIso(Iso), (#), _Unwrapped, _Wrapped', (&), from)
import Data.Functor.Identity(Identity(Identity, runIdentity))
import Data.Geodetic.ECEF(ECEF(..), HasECEF(z))
import Data.Geodetic.Ellipsoid(Ellipsoid, HasEllipsoid(semiMajor, flattening), flatteningReciprocal, wgs84)
import Data.Geodetic.LL(LL(LL), HasLL(lat, lon))
import Data.Geodetic.LLH(LLH(LLH), HasLLH(height))
import Data.Geodetic.XY(XY(XY), HasXY(x, y))

-- $setup
-- >>> import Control.Lens(ReifiedIso(runIso))
-- >>> import Data.Geodetic.Ellipsoid

newtype EllipsoidReaderT f a =
  EllipsoidReaderT (Ellipsoid -> f a)

makeWrapped ''EllipsoidReaderT

type EllipsoidReader a =
  EllipsoidReaderT Identity a

runEllipsoidReader ::
  Iso'
    (EllipsoidReader a)
    (Ellipsoid -> a)
runEllipsoidReader =
  iso
    (\(EllipsoidReaderT k) -> runIdentity . k)
    (\k -> EllipsoidReaderT (Identity . k))

toEllipsoidReaderT ::
  Iso'
    (EllipsoidReaderT f a)
    (ReaderT Ellipsoid f a)
toEllipsoidReaderT =
  from (_Wrapped' . from _Wrapped')

hoistEllipsoidReader ::
  Applicative f =>
  EllipsoidReader a
  -> EllipsoidReaderT f a
hoistEllipsoidReader (EllipsoidReaderT k) =
  EllipsoidReaderT (pure . runIdentity . k)

arrEllipsoidReader ::
  Applicative f =>
  (Ellipsoid -> a)
  -> EllipsoidReaderT f a
arrEllipsoidReader k =
  EllipsoidReaderT (pure . k)

instance Functor f => Functor (EllipsoidReaderT f) where
  fmap f (EllipsoidReaderT k) =
    EllipsoidReaderT (fmap f . k)

instance Applicative f => Applicative (EllipsoidReaderT f) where
  pure =
    EllipsoidReaderT . pure . pure
  EllipsoidReaderT f <*> EllipsoidReaderT a =
      EllipsoidReaderT (liftA2 (<*>) f a)

instance Monad f => Monad (EllipsoidReaderT f) where
  return =
    EllipsoidReaderT . Monad.return . Monad.return
  EllipsoidReaderT k >>= f =
    EllipsoidReaderT (\e -> k e Monad.>>= \q -> e & f q ^. _Wrapped')

instance Alternative f => Alternative (EllipsoidReaderT f) where
  empty =
    EllipsoidReaderT (\_ -> empty)
  EllipsoidReaderT a <|> EllipsoidReaderT b =
    EllipsoidReaderT (liftA2 (<|>) a b)

instance MonadPlus f => MonadPlus (EllipsoidReaderT f) where
  mzero =
    EllipsoidReaderT (\_ -> mzero)
  EllipsoidReaderT a `mplus` EllipsoidReaderT b =
    EllipsoidReaderT (liftA2 mplus a b)

instance MonadTrans EllipsoidReaderT where
  lift =
    EllipsoidReaderT . pure

instance MonadIO f => MonadIO (EllipsoidReaderT f) where
  liftIO =
    EllipsoidReaderT . pure . liftIO 

instance MonadFix f => MonadFix (EllipsoidReaderT f) where
  mfix f =
    EllipsoidReaderT (\e -> mfix (\q -> e & f q ^. _Wrapped'))

instance MonadZip f => MonadZip (EllipsoidReaderT f) where
  EllipsoidReaderT a `mzip` EllipsoidReaderT b =
    EllipsoidReaderT (liftA2 mzip a b)

readEllipsoid ::
  Applicative f =>
  EllipsoidReaderT f Ellipsoid
readEllipsoid =
  EllipsoidReaderT pure

readSemiMajor ::
  Applicative f =>
  EllipsoidReaderT f Double
readSemiMajor =
  (^. semiMajor) <$> readEllipsoid

readFlattening ::
  Applicative f =>
  EllipsoidReaderT f Double
readFlattening =
  (^. flattening) <$> readEllipsoid

readFlatteningReciprocal ::
  Applicative f =>
  EllipsoidReaderT f Double
readFlatteningReciprocal =
  (^. flatteningReciprocal) <$> readEllipsoid

semiMinor ::
  Applicative f =>
  EllipsoidReaderT f Double
semiMinor =
  (\f m -> m * (1 - f)) <$>
  readFlatteningReciprocal <*>
  readSemiMajor

-- |
--
-- >>> wgs84' eccentricitySquared
-- 6.6943799901413165e-3
eccentricitySquared ::
  Applicative f =>
  EllipsoidReaderT f Double
eccentricitySquared =
  (\f -> 2 * f - (f * f)) <$> readFlatteningReciprocal

-- |
--
-- >>> wgs84' eccentricitySquared'
-- 6.694455244784511e-3
eccentricitySquared' ::
  Applicative f => 
  EllipsoidReaderT f Double
eccentricitySquared' =
  (\f -> (f * (2 - f)) / (1 - f * f)) <$> readFlatteningReciprocal

distributeNormal ::
  Applicative f =>
  Double
  -> EllipsoidReaderT f Double
distributeNormal t =
  (\k -> k t) <$> normal

-- |
--
-- >>> wgs84' normal 7
-- 6387371.845852088
--
-- >>> wgs84' normal 71
-- 6397535.266650572
--
-- >>> wgs84' normal 711
-- 6393308.675975408
--
-- >>> wgs84' normal (-7)
-- 6387371.845852088
normal ::
  Applicative f =>
  EllipsoidReaderT f (Double -> Double)
normal =
  (\s m t -> m / sqrt (1 - s * sin t ^ (2 :: Int))) <$> eccentricitySquared <*> readSemiMajor

wgs84' ::
  EllipsoidReader a
  -> a
wgs84' r =
  (r ^. runEllipsoidReader) wgs84

wgs84'' ::
  EllipsoidReaderT f a
  -> f a
wgs84'' r =
  (_Unwrapped # r) wgs84

-- |
--
-- >>> ECEF (XY (-5019624) 2618621) (-2927516) ^. runIso (wgs84' earthGeo)
-- LLH {ll = LL {_lat = -0.4799654447089294, _lon = 2.66075442877903}, _height = 100.20987554546446}
--
-- >>> ECEF (XY 9919623 (-3116612)) (-2396517) ^. runIso (wgs84' earthGeo)
-- LLH {ll = LL {_lat = -0.22740831363634992, _lon = -0.30442061911398305}, _height = 4293252.6636643605}
--
-- >>> LLH (LL 0.48 2.661) 100 ^. from (runIso (wgs84' earthGeo))
-- ECEF {_xy = XY {_x = -5020176.908575072, _y = 2617341.3240995244}, _z = 2927710.5079646683}
--
-- >>> LLH (LL (-0.22741) (-0.30442)) 4293252.66 ^. from (runIso (wgs84' earthGeo))
-- ECEF {_xy = XY {_x = 9919621.069754401, _y = -3116604.645933256}, _z = -2396534.4668575544}
earthGeo ::
  Applicative f =>
  EllipsoidReaderT f (ReifiedIso' ECEF LLH)
earthGeo =
  let f e2 a nt =
        Iso (
          iso
            (\ecef -> 
              let 
                  x_ = ecef ^. x
                  y_ = ecef ^. y
                  h_ = ecef ^. z
                  sq q = q ^ (2 :: Int)
                  p2 = sq x_ + sq y_
                  a2 = sq a
                  e4 = sq e2
                  zeta = (1 - e2) * (sq h_ / a2)
                  rho = (p2 / a2 + zeta - e4) / 6
                  rho2 = sq rho
                  rho3 = rho * rho2
                  s = e4 * zeta * p2 / (4 * a2)
                  cbrt q = q ** (1 / 3)
                  t = cbrt (s + rho3 + sqrt (s * (s + 2 * rho3)))
                  u = rho + t + rho2 / t
                  v = sqrt (sq u + e4 * zeta)
                  w = e2 * (u + v - zeta) / (2 * v)
                  kappa = 1 + e2 * (sqrt (u + v + sq w) + w) / (u + v)
                  phi = atan (kappa * h_ / sqrt p2)
                  norm = nt phi
                  l = h_ + e2 * norm * sin phi
              in LLH (LL phi (atan2 y_ x_)) (sqrt (l ^ (2 :: Int) + p2) - norm))
            (\llh ->
              let t_ = llh ^. lat
                  n_ = llh ^. lon
                  h_ = llh ^. height
                  n = nt t_
                  cs k = (n + h_) * cos t_ * k n_
                  z_ = (n * (1 - e2) + h_) * sin t_
              in ECEF (XY (cs cos) (cs sin)) z_)
        )
  in  f <$>
      eccentricitySquared <*>
      readSemiMajor <*>
      normal
