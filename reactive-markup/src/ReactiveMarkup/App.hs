module ReactiveMarkup.App where

import Data.Data
import Data.Functor.Identity
import Optics.Core
import ReactiveMarkup.Contexts.Base
import ReactiveMarkup.Markup hiding (Empty)
import Control.Monad (join)

data App s t e = App
  { appRender :: Dynamic t s -> Markup t Root e,
    appHandleEvent :: e -> s -> IO s,
    appInitialState :: s
  }

-- data StateHook s e = StateHook
--   { shHandleEvent :: e -> IO ()
--   , shGetState :: IO s
--   , shRegisterRedraw :: IO () -> IO ()
--   }

-- data InitUI s t e = InitUI
--   { uiStateHook :: StateHook s e
--   , uiRender :: Dynamic t s -> Markup t Root e
--   }

-- ioRefInitUI :: s -> (e -> s -> Maybe s) -> IO (StateHook s e)

-- data ListStore a f = ListStore (f [f a])

-- data ShowF (m :: (* -> *) -> *) (f :: * -> *) = forall a. Show (f a) => ShowF (m f)

-- deriving instance Show (ShowF f a)

data Model f = Model
  { simple2 :: f (Simple String f),
    simple2 :: f (List Int f)
  }

newtype List a f = List [f a] deriving Show

data None a = None

class ZipTraverseF (x :: (* -> *) -> *) where
  traverseF :: Applicative m => 
    (forall a b. (a -> m b) -> f a -> m (g b)) ->
    x f -> m (x g)
  -- zipF :: (forall a b c. f a -> g b -> h (b,d)) -> x f -> x g -> x h
  foldF2 :: Semigroup m => (forall a b. f (a f) -> g (a g) -> m) -> x f -> x g -> m 
  emptyF :: x None

-- class ZipMapF (m :: (* -> *) -> *) where
--   zipMapF :: (forall a b. (a -> b) -> f a -> g a -> h b) -> m f -> m g -> m h

instance ZipTraverseF Model where
  -- emptyF = Model None None
  -- traverseF f (Model s1 s2) =
  --   let g1 = f pure s1
  --       g2 = f (traverse (f pure)) s2
  --    in Model <$> g1 <*> g2
  foldF2 f (Model s1a s1b) (Model s2a s2b) = 
    f s1a s2a <> f s1b s2b
  -- zipF f (Model s1a s1b) (Model s2a s2b) =
  --   Model (f s1a s2a) (f s1b s2b)

testFoldF2 :: Model Print -> Model Identity -> IO ()
testFoldF2 = foldF2 $ \a b -> pure ()

data Print a = Print (a -> IO ())

newtype Simple a (f :: * -> *) = Simple a deriving Show

mapF :: (ZipTraverseF x) => (forall a b. (a -> b) -> f a -> g b) -> x f -> x g
mapF f = runIdentity . traverseF (\f' -> pure . f (runIdentity . f'))

newtype Wrap a f = Wrap {unwrap :: f a}

instance ZipTraverseF (Wrap a) where
  emptyF = Wrap None
  traverseF to (Wrap x) = Wrap <$> to pure x

data Update a = Update a (a -> IO ())

-- testMapF :: Model Identity -> Model IO
-- testMapF = mapF $ \f fa -> pure $ runIdentity (f <$> fa)

-- testTraverseF :: Model IO -> IO (Model Set)
-- testTraverseF = traverseF $ \f io -> Keep <$> (io >>= f)

-- listStoreElem :: Int -> Lens' (ListStore String f) String
-- listStoreElem i =





























-- class DeepGet a b | a -> b where
--   deepGet :: a -> b

-- instance {-# INCOHERENT #-} DeepGet (Set a) a where
--   deepGet (Keep a) = a
--   deepGet (Set a) = a

-- instance DeepGet (Set a) b => DeepGet (Set (Set a)) b where
--   deepGet (Keep a) = deepGet a
--   deepGet (Set a) = deepGet a

-- class DeepSet a b s | a s -> b where
--   deepSet :: s -> a -> b

-- -- instance  {-# INCOHERENT #-} DeepSet (Set a) (Set s) s where
-- --   deepSet s _ = Set s

-- instance {-# INCOHERENT #-} DeepSet (Set a) (Set a) a where
--   deepSet s _ = Set s

-- instance DeepSet (Set a) (Set b) s => DeepSet (Set (Set a)) (Set (Set b)) s where
--   deepSet s (Keep a) = Keep $ deepSet s a
--   deepSet s (Set a) = Set $ deepSet s a

-- test :: Set a -> a
-- test = deepGet

-- test2 :: a -> Set (Set (Set a)) -> Set (Set (Set a))
-- test2 s f =
--   let x = deepSet s f
--    in undefined

-- deepLens :: (DeepSet f f a, DeepGet f a) => Lens' f a
-- deepLens = lens deepGet (flip deepSet)

-- -- nested :: Lens' (Set (Set a)) a
-- -- nested = lens (get . get) set
-- --   where
-- --     get :: forall a. Set a -> a
-- --     get (Set a) = a
-- --     get (Keep a) = a
-- --     set _ a = Keep $ Set a

-- setL :: NotSet a => Lens' (Set a) a
-- setL = lens get set
--   where
--     get (Set a) = a
--     get (Keep a) = a
--     set _ a = Set a

-- type family NotSet a where
--   NotSet (Set a) = InvalidUseOfSetL
--   NotSet (f a b c d) = (NotSet c, NotSet a, NotSet b, NotSet d)
--   NotSet (f a b c) = (NotSet c, NotSet a, NotSet b)
--   NotSet (f a b) = (NotSet a, NotSet b)
--   NotSet (f a) = NotSet a
--   NotSet _ = ()

-- class InvalidUseOfSetL

-- data Set a = Set a | Keep a deriving (Show)

-- testM :: Model Set -> Model Set
-- testM (Model simple n) =
--   let t = undefined -- (n ^. nested)
--    in Model (simple & setL .~ "Hello") n

-- model :: Model Set
-- model = Model (Keep "hello") undefined
