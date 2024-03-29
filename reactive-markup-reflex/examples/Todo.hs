{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.Coerce
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void
import GHC.Generics
import Optics.Core
import ReactiveMarkup
import ReactiveMarkup.Target.ReflexDom
import ReactiveMarkup.Target.ReflexDom (runReflexDom)

-- Start the app

main :: IO ()
main = runReflexDomWarp app

app :: App RDom TodoModel AppEvent
app =
  App
    { appName = "Todos",
      appRender = lift . renderView,
      appHandleEvent = handleEvent,
      appInitialState = initialModel
    }

-- Model definition

newtype TodoModel f = TodoModel
  { todos :: f (Nested (List (Nested Todo)))
  }
  deriving (Generic)

deriving instance Show (TodoModel ID)

data Todo f = Todo
  { todoText :: f (Direct Text),
    todoDone :: f (Direct Bool)
  }
  deriving (Generic)

deriving instance Show (Todo ID)

instance TransformFData TodoModel where
  transformFData fD fN (TodoModel todos1) (TodoModel todos2) = TodoModel <$> fN todos1 todos2

instance TransformFData Todo where
  transformFData fD fN (Todo t1 d1) (Todo t2 d2) = Todo <$> fD t1 t2 <*> fD d1 d2

initialModel :: TodoModel ID
initialModel = TodoModel $ coerce @[Todo ID] $ [Todo ("Get tea" ^. upwards) (False ^. upwards)]

-- Render View

renderView :: TodoModel (DynamicF RDom) -> Markup RDom Common AppEvent
renderView model =
  let todos' = todos model ^. deeper % mapping #children
      renderedTodos = dynamicMarkup todos' $ \ts -> column $ uncurry renderTodo <$> zip [0 ..] ts
   in column
        [ renderedTodos,
          button [#click ?~ AddTodo] "Add Todo",
          button [#click ?~ PrintTodos] "Print Todos"
        ]

renderTodo :: Int -> DynamicF RDom (Nested Todo) -> Markup RDom Common AppEvent
renderTodo nr todo =
  let done = view deeper todo >>= view (#todoDone % deeper)
      dText = view deeper todo >>= view (#todoText % deeper)
   in row $
        [ dynamicMarkup done $ \d -> button [(#click ?~ FlipChecked nr)] (if d then bold "[X]" else bold "[ ]"),
          textField [(#change ?~ SetText nr)] dText,
          button [(#click ?~ DeleteTodo nr)] "Delete"
        ]

-- Handling app events

data AppEvent = SetText Int Text | FlipChecked Int | AddTodo | DeleteTodo Int | PrintTodos

handleEvent :: AppEvent -> ModelM TodoModel IO ()
handleEvent appEvent = case appEvent of
  SetText nr txt -> mPut (todo nr % deeper % #todoText) txt
  FlipChecked nr -> mModify (todo nr % deeper % #todoDone) not
  DeleteTodo nr -> mModify #todos (#children %~ (\c -> take nr c <> drop (nr + 1) c))
  AddTodo -> do
    mModify
      #todos
      (#children %~ (++ [Todo ("New Todo" ^. upwards) (False ^. upwards) ^. upwards]))
  PrintTodos -> do
    todos <- mGet #todos
    liftIO $
      forM_ (todos ^. #children) $ \todo -> do
        let text = todo ^. deeper % #todoText % deeper
            done = todo ^. deeper % #todoDone % deeper
        T.putStrLn $ (if done then "- [X] " else "- [ ] ") <> text
    liftIO $ T.putStrLn ""
  where
    todo :: Int -> AffineTraversal' (TodoModel Update) (Update (Nested Todo))
    todo nr = #todos % deeper % #children % ix nr