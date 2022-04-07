module ReactiveMarkup.App where

import Data.Data
import Data.Functor.Identity
import Optics.Core
import ReactiveMarkup.Contexts.Base
import ReactiveMarkup.Markup hiding (Empty)
import Control.Monad (join, forM_)
import Data.IntMap
import Data.Coerce

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

-- data Model f = Model
--   { simple1 :: f String,
--     simple2 :: f ([f Int]) 
--   }

-- data Test (a :: Hyper -> *) = Test (a IdentityF)

-- data Hyper = Loop ((Hyper -> *) -> *)

-- data PrintTest (f :: Hyper) = PrintTest (f PrintTest -> IO ())


-- data PrintF (a :: Hyper -> *) = PrintF (a (Loop PrintF) -> IO ())

-- data IdentityF a (hyper :: Hyper -> *) = IdentityF a

-- data SimpleF a (b :: Loop f) = SimpleWhenMatchedF 

-- data TestModel (a :: Hyper) = TestModel {
--     testModel1 :: Int
--   }

-- printTest :: PrintF String
-- printTest = undefined 

-- test :: IdentityF String hyper
-- test = "hello"

-- data Test123 (a :: Hyper) where
--   Test123 :: End
 
-- type family Direct (f :: * -> *) where
--   Direct f = f

-- type family Nested (f :: * -> *) (c :: (* -> *) -> *) :: *

-- type instance Nested Print c = (Print (c Identity), c Print)

-- newtype List a f = List (IntMap (f a)) deriving Show

-- data None a = None

-- class ZipTraverseF (x :: (* -> *) -> *) where
--  traverseF :: Applicative m => 
--    (forall a b. (a -> m b) -> f a -> m (g b)) ->
--    x f -> m (x g)
  -- -- zipF :: (forall a b c. f a -> g b -> h (b,d)) -> x f -> x g -> x h
  -- foldF2 :: Semigroup m => (forall a b. f a -> g a -> m) -> x f -> x g -> m 
  -- emptyF :: x None

-- class ZipMapF (m :: (* -> *) -> *) where
--   zipMapF :: (forall a b. (a -> b) -> f a -> g a -> h b) -> m f -> m g -> m h

-- instance ZipTraverseF Model where
--   -- emptyF = Model None None
--   traverseF f (Model s1 s2) =
--     let g1 = f pure s1
--         g2 = f (traverse (f pure)) s2
--      in Model <$> g1 <*> g2
-- --   foldF2 f (Model s1a s1b) (Model s2a s2b) = 
--     f s1a s2a <> f s1b s2b
--   -- zipF f (Model s1a s1b) (Model s2a s2b) =
--   --   Model (f s1a s2a) (f s1b s2b)


-- instance ZipTraverseF (IdentityF a) where
--   traverseF f (IdentityF a) = IdentityF <$> f pure a

-- testFoldF2 :: Model Print -> Model Identity -> IO ()
-- testFoldF2 = foldF2 $ \a b -> pure ()


-- test :: Model Identity -> Model Print
-- test = mapF $ \f (Identity a) -> Print (print) (f a) 

-- works = "WORKS!!!"

newtype F = FRec ((F -> *) -> *)

type family ApplyF (f :: F) (a :: F -> *) where
    ApplyF (FRec f) a = f a

newtype SimpleF a (f :: F) = SimpleF a

newtype ListF (a :: F -> *) (f :: F) = ListF [ApplyF f a]

data TModel (f :: F) = TModel {
  tm1 :: ApplyF f (SimpleF Int),
  tm2 :: ApplyF f (ListF (SimpleF Int))
}

test :: TModel (FRec IdentityF)
test = TModel (coerce @Int 3) (coerce @[Int] [2])

test2 :: IdentityF TModel
test2 = IdentityF test

newtype IdentityF (a :: F -> *) = IdentityF (a (FRec IdentityF))

newtype IOF (a :: F -> *) = IOF (IO (a (FRec IOF)))

newtype PrintF (a :: F -> *) = PrintF (a (FRec IdentityF) -> IO ())



class TraverseF (x :: F -> *) where
 traverseF :: Applicative m => 
   (forall a. TraverseF a => f a -> m (g a)) ->
   x (FRec f) -> m (x (FRec g))

instance TraverseF TModel where
  -- emptyF = Model None None
  traverseF f (TModel s1 s2) =
    let g1 = f s1
        g2 = f s2
     in TModel <$> g1 <*> g2

instance TraverseF (SimpleF a) where
  traverseF f (SimpleF a) = pure $ SimpleF a


instance TraverseF x => TraverseF (ListF x) where
  traverseF f (ListF elems) = ListF <$> traverse f elems
  
  
tryTraverseF :: TraverseF x => x (FRec IOF) -> IO (x (FRec IdentityF))
tryTraverseF = traverseF $ \(IOF ioA) -> ioA >>= (fmap IdentityF . tryTraverseF)


-- newtype F = Arg (F -> * *)

-- data family ApplyF (m :: ((F -> *) -> *)) (f :: F)

-- newtype instance ApplyF m (Arg f) = ApplyF (m f)


-- -- data family ApplyF2 (f :: F) a 

-- -- newtype instance ApplyF2 (Arg f) a = ApplyF2 ()


-- newtype SimpleF a (f :: F) = SimpleF a


-- data TModel (a :: F -> *) = TModel {
--   tm1 :: a (Arg a),
--   tm2 :: Int
-- }

-- test :: TModel (ApplyF IdentityF)
-- test = TModel (IdentityF $ SimpleF 3) 3

-- test2 :: IdentityF TModel
-- test2 = IdentityF $ (ApplyF test)

-- newtype IdentityF (a :: F -> *) = IdentityF (a (Arg (ApplyF IdentityF)))



data Print a = Print (a -> IO ()) a 

-- type family PrintT a where
--   PrintT (Simple a Print) = a -> IO ()
--   PrintT (f a Print) = f a Print

-- newtype Simple a (f :: * -> *) = Simple a deriving Show

-- mapF :: ZipTraverseF x => (forall a b. (a -> b) -> f a -> g b) -> x f -> x g
-- mapF f = runIdentity . traverseF (\f' -> pure . f (runIdentity . f'))

-- newtype Wrap a f = Wrap {unwrap :: f a}

-- instance ZipTraverseF (Wrap a) where
--   emptyF = Wrap None
--   traverseF to (Wrap x) = Wrap <$> to pure x

-- data Update a = Update a (a -> IO ())

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
