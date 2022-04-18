module ReactiveMarkup.App where
import ReactiveMarkup.Context (Root)
import ReactiveMarkup.Markup (DynamicF, Markup)
import Data.Text ( Text )
import Optics.Core
import Data.Coerce
import Unsafe.Coerce
import Data.RHKT
import GHC.Generics

data App t (s :: FData) e = App
  { appRender :: s (DynamicF t) -> Markup t Root e,
    appHandleEvent :: e -> Model s -> IO (Model s),
    appInitialState :: s ID,
    appName :: Text
  }

-- 

data Update (f :: F) =
  UpdateKeep (ApplyF f Update) |
  UpdatePropagate (ApplyF f Update) |
  UpdateSet (ApplyF f ID)

deriving instance (Show (ApplyF f Update), Show (ApplyF f ID)) => Show (Update f)

newtype Model a = Model { getInternalModel :: a Update }

deriving instance (Show (a Update)) => Show (Model a)

instance Deeper Update where
  type Deep Update a = ApplyF a Update
  type DeeperC Update a = ZipTraverseF (Wrap a)
  deeper = lens get set
    where
      get :: forall f. ZipTraverseF (Wrap f) => Update f -> ApplyF f Update
      get (UpdateKeep a) = a
      get (UpdatePropagate a) = a
      get (UpdateSet a) = get $ wrapped $ toNewUpdate $ Wrap (ID a :: ID f)
      set (UpdateSet a) _ = UpdateSet a
      set _ a = UpdatePropagate a

toNewUpdate :: ZipTraverseF x => x ID -> x Update
toNewUpdate = mapF (\(ID a) -> UpdateKeep a) (\(ID a) -> UpdateKeep (toNewUpdate a))

toID :: ZipTraverseF x => x Update -> x ID
toID = mapF (ID . getD) (ID . getN)
  where
    getN (UpdateKeep a) = toID a
    getN (UpdatePropagate a) = toID a
    getN (UpdateSet a) = a
    getD (UpdateKeep a) = a
    getD (UpdatePropagate a) = a
    getD (UpdateSet a) = a

modelSet :: (Is k An_AffineTraversal) => Optic' k ix (a Update) (Update b) -> ApplyF b ID -> Model a -> Model a
modelSet l n (Model m) = Model $ m & withAffineTraversal l atraversal .~ UpdateSet n

modelModify :: (ZipTraverseF (Wrap b), Is k An_AffineTraversal) => Optic' k ix (a Update) (Update b) -> (ApplyF b ID -> ApplyF b ID) -> Model a -> Model a
modelModify l f (Model m) = Model $ m & withAffineTraversal l atraversal %~ (UpdateSet . f . runID . wrapped . toID . Wrap)

modelView :: ZipTraverseF (Wrap b) => Model a -> Lens' (a Update) (Update b) -> ApplyF b ID
modelView (Model m) l = let (ID x) = wrapped $ toID $ Wrap $ m ^. l in x

modelPreview :: (ZipTraverseF (Wrap b), Is k An_AffineTraversal) => Model a -> Optic' k ix (a Update) (Update b) -> Maybe (ApplyF b ID)
modelPreview (Model m) l = (\a -> let (ID x) = wrapped $ toID $ Wrap a in x) <$> preview (withAffineTraversal l atraversal) m


data UpdateF r (f :: F) = UpdateF (ApplyF f (UpdateF r)) (ApplyF f ID -> r)

-- nested :: UpdateF r f -> ApplyF f (UpdateF r)
-- nested (UpdateF a _) = a

-- update :: UpdateF r f -> ApplyF f ID -> r
-- update (UpdateF _ a) = a

-- data Keep a = Keep a | Set a | Propagate a

-- getKeep :: Keep a -> a
-- getKeep (Keep a) = a
-- getKeep (Set a) = a
-- getKeep (Propagate a) = a

-- newtype Put a = Put a

-- data SimpleTest = SimpleTest {
--   test123 :: Int
-- } deriving Generic

-- test :: Change (Model NewUpdate) -> Change (Model NewUpdate)
-- test = modify (t %% deeper %% gfield @"simple1") 3
--   where 
--     t = gfield @"simple1"

-- data Model f = Model
--   { simple1 :: f (Nested Simple),
--     simple2 :: f (Nested (List (Direct Int)))
--   } deriving Generic

-- newtype Simple f = Simple {
--   simple1 :: f (Direct Int)
-- } deriving Generic

-- instance ZipTraverseF Model where

-- instance ZipTraverseF Simple where



-- iModel :: Model ID
-- iModel = Model (coerce @Int 3) (coerce @[Int] [1,2,3])

-- toUpdate :: ZipTraverseF x => x ID -> x UpdateF
-- toUpdate = mapF (\(ID a) -> UpdateF () (\_ -> pure ())) (\(ID a) -> UpdateF )

-- newtype UpdateEnv a =  UpdateEnv a deriving (Functor, Show)

-- -- getS :: S a -> a
-- -- getS (S a) = a
-- -- getS (K a) = a

-- data Update a = Keep a | Set a | Propagate a deriving Show

-- getUpdateValue :: Update a -> a
-- getUpdateValue (Keep a) = a
-- getUpdateValue (Set a) = a
-- getUpdateValue (Propagate a) = a

-- type UpdateF = FunctorF Update

-- nested :: Lens' (UpdateEnv (UpdateF f)) (ApplyF f UpdateF)
-- nested = lens get set
--   where
--     get (UpdateEnv (FunctorF (Set a))) = a
--     get (UpdateEnv (FunctorF (Keep a))) = a
--     get (UpdateEnv (FunctorF (Propagate a))) = a
--     set _ a = UpdateEnv $ FunctorF $ Set a

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


-- data Test (a :: Hyper -> *) = Test (a ID)

-- data Hyper = Loop ((Hyper -> *) -> *)

-- data PrintTest (f :: Hyper) = PrintTest (f PrintTest -> IO ())

-- data PrintF (a :: Hyper -> *) = PrintF (a (Loop PrintF) -> IO ())

-- data ID a (hyper :: Hyper -> *) = ID a

-- data Simple a (b :: Loop f) = SimpleWhenMatchedF

-- data TestModel (a :: Hyper) = TestModel {
--     testModel1 :: Int
--   }

-- printTest :: PrintF String
-- printTest = undefined

-- test :: ID String hyper
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

-- instance ZipTraverseF (ID a) where
--   traverseF f (ID a) = ID <$> f pure a

-- testFoldF2 :: Model Print -> Model Identity -> IO ()
-- testFoldF2 = foldF2 $ \a b -> pure ()

-- test :: Model Identity -> Model Print
-- test = mapF $ \f (Identity a) -> Print (print) (f a)

-- works = "WORKS!!!"

-- data Nested = N | D

-- newtype F = FRec ((F -> *) -> *)

-- type family ApplyF (f :: F) (a :: F -> *) where
--     ApplyF (FRec f) a = f a

-- data Wrap a f = Wrap  deriving Show

-- newtype Simple a (f :: F) = Simple ()

-- instance Show (ApplyF f (Wrap a)) => Show (Simple a f) where
--   show (Simple f) = show f

-- newtype List (a :: F -> *) (f :: F) = List [ApplyF f a]

-- instance Show (ApplyF f a) =>Show (List a f) where
--   show (List elems) = "List " ++ show elems

-- data TModel (f :: F) = TModel {
--   tm1 :: ApplyF f (Simple Int),
--   tm2 :: ApplyF f (List (Simple Int))
-- }

-- test :: TModel (FRec ID)
-- test = TModel (ID $ Simple (ID Wrap)) (coerce @[Int] [2])

-- test2 :: TModel (FRec PrintF)
-- test2 = TModel (PrintF print (Simple (PrintF print (Wrap 2)))) undefined

-- newtype ID (a :: F -> *) = ID (a (FRec ID))

-- instance Show (a (FRec ID)) => Show (ID a) where
--   show (ID a) = "ID " ++ show a

-- newtype IOF (a :: F -> *) = IOF (IO (a (FRec IOF)))

-- data PrintF (a :: F -> *) = PrintF (a (FRec ID) -> IO ()) (a (FRec PrintF))

-- instance ZipF (Simple a) where
--   zipF _ (Simple a) _ = Simple a

-- testzip2 :: ZipF a => a (FRec ID) -> a (FRec PrintF) -> a (FRec IOF)
-- testzip2 = zipF $ \(ID a) (PrintF f fd) -> IOF $ do
--   f a
--   pure $ testzip2 a fd

-- class TraverseF (x :: F -> *) where
--  traverseF :: Applicative m =>
--    (forall a. TraverseF a => f a -> m (g a)) ->
--    x (FRec f) -> m (x (FRec g))

-- class ZipF (x :: F -> *) where
--  zipF :: (forall a. ZipF a => f a -> g a -> h a) -> x (FRec f) -> x (FRec g) -> x (FRec h)

-- instance TraverseF TModel where
--   -- emptyF = Model None None
--   traverseF f (TModel s1 s2) =
--     let g1 = f s1
--         g2 = f s2
--      in TModel <$> g1 <*> g2

-- instance TraverseF (Simple a) where
--   traverseF f (Simple a) = pure $ Simple a

-- instance TraverseF x => TraverseF (List x) where
--   traverseF f (List elems) = List <$> traverse f elems

-- instance ZipF x => ZipF (List x) where
--   zipF f (List elems1) (List elems2) = List (f <$> elems1 <*> elems2)

-- data TwoF (x :: F -> *) = TwoF (x (FRec ID)) (x (FRec ID))

-- instance Show (x (FRec ID)) => Show (TwoF x) where
--   show (TwoF a b) = "TwoF " ++ show a ++ show b

-- ziptest :: List (Simple Int) (FRec ID)
-- ziptest = List [coerce @Int 3]

-- zipped :: List (Simple Int) (FRec TwoF)
-- zipped = zipF (\(ID a) (ID b) -> TwoF a b) ziptest ziptest

-- tryTraverseF :: TraverseF x => x (FRec IOF) -> IO (x (FRec ID))
-- tryTraverseF = traverseF $ \(IOF ioA) -> ioA >>= (fmap ID . tryTraverseF)

-- newtype F = Arg (F -> * *)

-- data family ApplyF (m :: ((F -> *) -> *)) (f :: F)

-- newtype instance ApplyF m (Arg f) = ApplyF (m f)

-- -- data family ApplyF2 (f :: F) a

-- -- newtype instance ApplyF2 (Arg f) a = ApplyF2 ()

-- newtype Simple a (f :: F) = Simple a

-- data TModel (a :: F -> *) = TModel {
--   tm1 :: a (Arg a),
--   tm2 :: Int
-- }

-- test :: TModel (ApplyF ID)
-- test = TModel (ID $ Simple 3) 3

-- test2 :: ID TModel
-- test2 = ID $ (ApplyF test)

-- newtype ID (a :: F -> *) = ID (a (Arg (ApplyF ID)))

-- data Print a = Print (a -> IO ()) a

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
