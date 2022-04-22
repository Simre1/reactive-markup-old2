module ReactiveMarkup.Update where

import Data.RHKT
import Optics.Core
import Data.Data

data Update (f :: F)
  = UpdateKeep (ApplyF f Update)
  | UpdatePropagate (ApplyF f Update)
  | UpdateSet (ApplyF f ID)

deriving instance (Show (ApplyF f Update), Show (ApplyF f ID)) => Show (Update f)

newtype Model a = Model {getInternalModel :: a Update}

deriving instance (Show (a Update)) => Show (Model a)

instance Deeper Update where
  type Deep Update a = ApplyF a Update
  type DeeperC Update a = (ZipTraverseF (Wrap a), WhichF a)
  deeper = lens get set
    where
      get :: forall f. ZipTraverseF (Wrap f) => Update f -> ApplyF f Update
      get (UpdateKeep a) = a
      get (UpdatePropagate a) = a
      get (UpdateSet a) = get $ wrapped $ toUpdate' $ Wrap (ID a :: ID f)
      
      set :: forall f. WhichF f => Update f -> ApplyF f Update -> Update f
      set (UpdateSet a) _ = UpdateSet a
      set _ a = case whichF @f of
              IsDirect -> UpdateSet a
              IsNested -> UpdatePropagate a

      setUpdateWrap :: ZipTraverseF x => x Update -> x Update
      setUpdateWrap = mapF (\a -> UpdateSet $ get a) (\a -> UpdatePropagate $ get a)
        
      toUpdate' :: ZipTraverseF x => x ID -> x Update
      toUpdate' = mapF (\(ID a) -> UpdateKeep a) (\(ID a) -> UpdateKeep (toUpdate' a))

toUpdate :: ZipTraverseF x => x ID -> x Update
toUpdate = mapF (\(ID a) -> UpdateSet a) (\(ID a) -> UpdateSet a)

toID :: ZipTraverseF x => x Update -> x ID
toID = mapF (ID . getD) (ID . getN)
  where
    getN (UpdateKeep a) = toID a
    getN (UpdatePropagate a) = toID a
    getN (UpdateSet a) = a
    getD (UpdateKeep a) = a
    getD (UpdatePropagate a) = a
    getD (UpdateSet a) = a

modelSet :: ApplyF x ID -> Update x
modelSet n = UpdateSet n

-- modelModify :: (ZipTraverseF (Wrap b), Is k An_AffineTraversal) => Optic' k ix (a Update) (Update b) -> (ApplyF b ID -> ApplyF b ID) -> Model a -> Model a
-- modelModify l f (Model m) = Model $ m & withAffineTraversal l atraversal %~ (UpdateSet . f . runID . wrapped . toID . Wrap)

-- modelView :: ZipTraverseF (Wrap b) => Model a -> Lens' (a Update) (Update b) -> ApplyF b ID
-- modelView (Model m) l = let (ID x) = wrapped $ toID $ Wrap $ m ^. l in x

-- modelPreview :: (ZipTraverseF (Wrap b), Is k An_AffineTraversal) => Model a -> Optic' k ix (a Update) (Update b) -> Maybe (ApplyF b ID)
-- modelPreview (Model m) l = (\a -> let (ID x) = wrapped $ toID $ Wrap a in x) <$> preview (withAffineTraversal l atraversal) m

        --   modelUpdate <- modelToUpdate model
        --   let LocalUpdate modelUpdate' outerEvent = update innerEvent (LocalUpdate (Model modelUpdate) Nothing)
        --   updateModel model (getInternalModel modelUpdate')