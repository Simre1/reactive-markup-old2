module Data.RHKT where

import Control.Monad (zipWithM)
import Data.Functor.Identity (Identity (runIdentity))
import GHC.Generics (Generic)

type FData = ((F -> *) -> *)

data F = Nested ((F -> *) -> *) | Direct *

class ZipTraverseF (t :: (F -> *) -> *) where
  zipTraverseF ::
    Applicative m =>
    (forall a. f (Direct a) -> g (Direct a) -> m (h (Direct a))) ->
    (forall a. ZipTraverseF a => f (Nested a) -> g (Nested a) -> m (h (Nested a))) ->
    t f ->
    t g ->
    m (t h)

type family ApplyF (a :: F) (t :: (F -> *)) where
  ApplyF (Nested f) t = f t
  ApplyF (Direct v) t = v

type family ApplyOnlyNested (a :: F) (t :: (F -> *)) where
  ApplyOnlyNested (Nested f) t = f t
  ApplyOnlyNested (Direct v) t = ()

traverseF :: (Applicative m, ZipTraverseF x) => (forall a. f (Direct a) -> m (g (Direct a))) -> (forall a. ZipTraverseF a => f (Nested a) -> m (g (Nested a))) -> x f -> m (x g)
traverseF fD fN x = zipTraverseF (\d1 _ -> fD d1) (\n1 _ -> fN n1) x x

mapF :: ZipTraverseF x => (forall a. f (Direct a) -> g (Direct a)) -> (forall a. ZipTraverseF a => f (Nested a) -> g (Nested a)) -> x f -> x g
mapF fD fN = runIdentity . traverseF (pure . fD) (pure . fN)

zipF :: ZipTraverseF x => (forall a. f (Direct a) -> g (Direct a) -> h (Direct a)) -> (forall a. ZipTraverseF a => f (Nested a) -> g (Nested a) -> h (Nested a)) -> x f -> x g -> x h
zipF fD fN xf xg = runIdentity $ zipTraverseF (\a b -> pure $ fD a b) (\a b -> pure $ fN a b) xf xg

foldF :: (ZipTraverseF x, Monoid m) => (forall a. f (Direct a) -> m) -> (forall a. ZipTraverseF a => f (Nested a) -> m) -> x f -> m
foldF fD fN x =
  let Fold m _ = traverseF (\a -> Fold (fD a) a) (\a -> Fold (fN a) a) x
   in m

foldF2 :: (ZipTraverseF x, Monoid m) => (forall a. f (Direct a) -> g (Direct a) -> m) -> (forall a. ZipTraverseF a => f (Nested a) -> g (Nested a) -> m) -> x f -> x g -> m
foldF2 fD fN x y =
  let Fold m _ = zipTraverseF (\a b -> Fold (fD a b) a) (\a b -> Fold (fN a b) a) x y
   in m

data Fold m a = Fold m a

instance Functor (Fold m) where
  fmap f (Fold m a) = Fold m (f a)

instance Monoid m => Applicative (Fold m) where
  pure = Fold mempty
  (Fold m1 f) <*> (Fold m2 a) = Fold (m1 <> m2) (f a)

newtype List (a :: F) (f :: F -> *) = List {children :: [f a]} deriving Generic

deriving instance Show (f a) => Show (List a f)

instance ZipTraverseF f => ZipTraverseF (List (Nested f)) where
  zipTraverseF fD fN (List elems1) (List elems2) = List <$> zipWithM fN elems1 elems2

instance ZipTraverseF (List (Direct a)) where
  zipTraverseF fD fN (List elems1) (List elems2) = List <$> zipWithM fD elems1 elems2

newtype IdentityF (a :: F) = IdentityF {runIdentityF :: ApplyF a IdentityF}

deriving instance Show (ApplyF a IdentityF) => Show (IdentityF a)

newtype FunctorF (f :: * -> *) (a :: F) = FunctorF {unF :: f (ApplyF a (FunctorF f))}

deriving instance Show (f (ApplyF a (FunctorF f))) => Show (FunctorF f a)

newtype Wrap (x :: F) (f :: F -> *) = Wrap {
  wrapped :: f x
}

instance ZipTraverseF (Wrap (Direct a)) where
  zipTraverseF fD _ (Wrap a) (Wrap b) = Wrap <$> fD a b

instance ZipTraverseF a => ZipTraverseF (Wrap (Nested a)) where
  zipTraverseF _ fN (Wrap a) (Wrap b) = Wrap <$> fN a b