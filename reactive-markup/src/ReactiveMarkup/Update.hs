{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax #-}

module ReactiveMarkup.Update where

import Control.Monad.IO.Class
import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State as S
import Data.Data
import Data.RHKT
import Data.String
import GHC.Generics
import qualified GHC.Records as R
import GHC.TypeLits
import Optics.Core
import qualified Optics.Internal.Generic
import qualified Optics.Internal.Generic as Optics.Internal.Generic.TypeLevel
import Prelude

data Update (f :: F)
  = UpdateKeep (ApplyF f Update)
  | UpdatePropagate (ApplyF f Update)
  | UpdateSet (ApplyF f ID)

deriving instance (Show (ApplyF f Update), Show (ApplyF f ID)) => Show (Update f)

instance Deeper Update where
  type Deep Update a = ApplyF a Update
  type DeeperC Update a = (TransformFData (Wrap a))
  deeper = lens get set
    where
      get :: forall f. TransformFData (Wrap f) => Update f -> ApplyF f Update
      get (UpdateKeep a) = a
      get (UpdatePropagate a) = a
      get (UpdateSet a) = get $ wrap $ toUpdate' $ Wrap (ID a :: ID f)

      set :: Update f -> ApplyF f Update -> Update f
      set (UpdateSet a) _ = UpdateSet a
      set _ a = UpdatePropagate a
      -- case whichF @f of
      -- IsDirect -> UpdateSet a
      -- IsNested -> UpdatePropagate a

      -- setUpdateWrap :: TransformFData x => x Update -> x Update
      -- setUpdateWrap = mapF (\a -> UpdateSet $ get a) (\a -> UpdatePropagate $ get a)

      toUpdate' :: TransformFData x => x ID -> x Update
      toUpdate' = mapF (\(ID a) -> UpdateKeep a) (\(ID a) -> UpdateKeep (toUpdate' a))

toUpdate :: TransformFData x => x ID -> x Update
toUpdate = mapF (\(ID a) -> UpdateSet a) (\(ID a) -> UpdateSet a)

toID :: TransformFData x => x Update -> x ID
toID = mapF (ID . getD) (ID . getN)
  where
    getN (UpdateKeep a) = toID a
    getN (UpdatePropagate a) = toID a
    getN (UpdateSet a) = a
    getD (UpdateKeep a) = a
    getD (UpdatePropagate a) = a
    getD (UpdateSet a) = a

-- data TestModel f = TestModel
--   { m1 :: f (Direct Int),
--     m2 :: f (Nested (List (Direct Int)))
--   }
--   deriving (Generic)

newtype ModelM s m a = ModelM {runModelM' :: StateT (s Update) m a} deriving (Functor, Applicative, Monad, MonadIO)

runModelM :: s Update -> ModelM s m a -> m (a, s Update)
runModelM s m = S.runStateT (runModelM' m) s

mPut :: (Monad m, Is k An_AffineTraversal) => Optic' k ix (s Update) (Update b) -> ApplyF b ID -> ModelM s m ()
mPut l n = ModelM $ S.modify $ withAffineTraversal l atraversal .~ UpdateSet n

mModify :: (TransformFData (Wrap b), Is k An_AffineTraversal, Monad m) => Optic' k ix (s Update) (Update b) -> (ApplyF b ID -> ApplyF b ID) -> ModelM s m ()
mModify l f = ModelM $ S.modify $ withAffineTraversal l atraversal %~ (UpdateSet . f . runID . wrap . toID . Wrap)

mGet :: (TransformFData (Wrap b), Monad m) => Lens' (s Update) (Update b) -> ModelM s m (ApplyF b ID)
mGet l = ModelM $ (\m -> runID $ wrap $ toID $ Wrap $ m ^. l) <$> S.get

mTryGet :: (TransformFData (Wrap b), Is k An_AffineTraversal, Monad m) => Optic' k ix (s Update) (Update b) -> ModelM s m (Maybe (ApplyF b ID))
mTryGet l = ModelM $ fmap (runID . wrap . toID . Wrap) . preview (withAffineTraversal l atraversal) <$> S.get

class GetField (label :: Symbol) b a where
  getField :: b -> a

class SetField (label :: Symbol) b a where
  setField :: b -> a -> b

data Test f = Test
  { hello1 :: f (Direct String),
    hello2 :: f (Nested (List (Direct Int)))
  }
  deriving (Generic)

instance {-# OVERLAPPABLE #-} GField label b b a a => GetField label b a where
  getField b = b ^. gfield @label

instance {-# OVERLAPPABLE #-} GField label b b a a => SetField label b a where
  setField b a = b & gfield @label .~ a

instance (ZipTraverseF (Wrap a), b ~ (ApplyF a Update)) => GetField "get" (Update a) b where
  getField b = b ^. deeper

testF :: Test Update -> Test Update
testF model =
  let x = model.hello1.get
   in model
