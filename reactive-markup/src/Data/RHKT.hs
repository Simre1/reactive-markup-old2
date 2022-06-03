{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
module Data.RHKT where

import Control.Monad (zipWithM)
import Data.Functor.Identity (Identity (runIdentity))
import Data.Kind (Constraint)
import GHC.Generics (Generic)
import Optics.Core hiding (Fold)
import Data.Data
import Unsafe.Coerce

type FData = ((F -> *) -> *)

data F = Nested ((F -> *) -> *) | Direct *

class TransformFData (t :: (F -> *) -> *) where
  transformFData ::
    Applicative m =>
    (forall a. f (Direct a) -> g (Direct a) -> m (h (Direct a))) ->
    (forall a. TransformFData a => f (Nested a) -> g (Nested a) -> m (h (Nested a))) ->
    t f ->
    t g ->
    m (t h)

type family ApplyF (a :: F) (t :: (F -> *)) where
  ApplyF (Nested f) t = f t
  ApplyF (Direct v) t = v

type family ApplyOnlyNested (a :: F) (t :: (F -> *)) where
  ApplyOnlyNested (Nested f) t = f t
  ApplyOnlyNested (Direct v) t = ()

traverseF :: (Applicative m, TransformFData x) => (forall a. f (Direct a) -> m (g (Direct a))) -> (forall a. TransformFData a => f (Nested a) -> m (g (Nested a))) -> x f -> m (x g)
traverseF fD fN x = transformFData (\d1 _ -> fD d1) (\n1 _ -> fN n1) x x

mapF :: TransformFData x => (forall a. f (Direct a) -> g (Direct a)) -> (forall a. TransformFData a => f (Nested a) -> g (Nested a)) -> x f -> x g
mapF fD fN = runIdentity . traverseF (pure . fD) (pure . fN)

zipF :: TransformFData x => (forall a. f (Direct a) -> g (Direct a) -> h (Direct a)) -> (forall a. TransformFData a => f (Nested a) -> g (Nested a) -> h (Nested a)) -> x f -> x g -> x h
zipF fD fN xf xg = runIdentity $ transformFData (\a b -> pure $ fD a b) (\a b -> pure $ fN a b) xf xg

foldF :: (TransformFData x, Monoid m) => (forall a. f (Direct a) -> m) -> (forall a. TransformFData a => f (Nested a) -> m) -> x f -> m
foldF fD fN x =
  let Fold m _ = traverseF (\a -> Fold (fD a) a) (\a -> Fold (fN a) a) x
   in m

foldF2 :: (TransformFData x, Monoid m) => (forall a. f (Direct a) -> g (Direct a) -> m) -> (forall a. TransformFData a => f (Nested a) -> g (Nested a) -> m) -> x f -> x g -> m
foldF2 fD fN x y =
  let Fold m _ = transformFData (\a b -> Fold (fD a b) a) (\a b -> Fold (fN a b) a) x y
   in m

data Fold m a = Fold m a

instance Functor (Fold m) where
  fmap f (Fold m a) = Fold m (f a)

instance Monoid m => Applicative (Fold m) where
  pure = Fold mempty
  (Fold m1 f) <*> (Fold m2 a) = Fold (m1 <> m2) (f a)

newtype List (a :: F) (f :: F -> *) = List {children :: [f a]} deriving (Generic)

deriving instance Show (f a) => Show (List a f)

instance TransformFData f => TransformFData (List (Nested f)) where
  transformFData fD fN (List elems1) (List elems2) = List <$> zipWithM fN elems1 elems2

instance TransformFData (List (Direct a)) where
  transformFData fD fN (List elems1) (List elems2) = List <$> zipWithM fD elems1 elems2

newtype ID (a :: F) = ID {runID :: ApplyF a ID}

deriving instance Show (ApplyF a ID) => Show (ID a)

newtype FunctorF (f :: * -> *) (a :: F) = FunctorF {unF :: f (ApplyF a (FunctorF f))}

deriving instance Show (f (ApplyF a (FunctorF f))) => Show (FunctorF f a)

newtype Wrap (x :: F) (f :: F -> *) = Wrap
  { wrap :: f x
  }
  deriving (Generic)

deriving instance Show (f x) => Show (Wrap x f)

instance TransformFData (Wrap (Direct a)) where
  transformFData fD _ (Wrap a) (Wrap b) = Wrap <$> fD a b

instance TransformFData a => TransformFData (Wrap (Nested a)) where
  transformFData _ fN (Wrap a) (Wrap b) = Wrap <$> fN a b

data EmptyF (f :: F -> *) = EmptyF deriving Show

instance TransformFData EmptyF where
  transformFData fD fN EmptyF EmptyF = pure EmptyF


class Deeper (f :: F -> *) where
  type Deep (f :: F -> *) (a :: F)
  type DeeperC (f :: F -> *) (a :: F) :: Constraint
  deeper :: DeeperC f a => Lens' (f a) (Deep f a)

class Upwards (f :: F -> *) where
  type Up (f :: F -> *) (a :: F)
  type UpC (f :: F -> *) (a :: F) :: Constraint
  upwards :: UpC f a => Lens' (Up f a) (f a)

instance Deeper ID where
  type Deep ID a = ApplyF a ID
  type DeeperC ID a = ()
  deeper = lens (\(ID a) -> a) (\_ a -> ID a)

instance Upwards ID where
  type Up ID a = ApplyF a ID
  type UpC ID a = ()
  upwards = lens (\a -> ID a) (\_ (ID a) -> a)

instance Deeper (FunctorF f) where
  type Deep (FunctorF f) a = f (ApplyF a (FunctorF f))
  type DeeperC (FunctorF f) a = ()
  deeper = lens (\(FunctorF a) -> a) (\_ a -> FunctorF a)

instance Applicative f => Upwards (FunctorF f) where
  type Up (FunctorF f) a = f (ApplyF a (FunctorF f))
  type UpC (FunctorF f) a = ()
  upwards = lens (\a -> FunctorF a) (\_ (FunctorF a) -> a)


data IsF f where
  IsDirect :: IsF (Direct x)
  IsNested :: IsF (Nested x)

class WhichF f where
  whichF :: IsF f

instance WhichF (Direct x) where
  whichF = IsDirect

instance WhichF (Nested x) where
  whichF = IsNested