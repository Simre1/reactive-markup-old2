{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

import Control.Monad (forM_)
import Data.Coerce
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Void
import GHC.Generics
import Optics.Core
import ReactiveMarkup
import ReactiveMarkup.Target.Gtk
import Control.Monad.IO.Class


-- Start the app

main :: IO ()
main = runGtk app

app :: App Gtk TodoModel AppEvent
app =
  App
    { appName = "Todos",
      appRender = renderView,
      appHandleEvent = handleEvent,
      appInitialState = initialModel
    }

-- Model definition

type TodoModel :: FData
newtype TodoModel f = TodoModel
  { todos :: f (Nested (List (Nested Todo)))
  }
  deriving (Generic)

deriving instance Show (TodoModel ID)

type Todo :: FData
data Todo f = Todo
  { todoText :: f (Direct Text),
    todoDone :: f (Direct Bool)
  }
  deriving (Generic)

deriving instance Show (Todo ID)

instance ZipTraverseF TodoModel where
  zipTraverseF fD fN (TodoModel todos1) (TodoModel todos2) = TodoModel <$> fN todos1 todos2

instance ZipTraverseF Todo where
  zipTraverseF fD fN (Todo t1 d1) (Todo t2 d2) = Todo <$> fD t1 t2 <*> fD d1 d2

initialModel :: TodoModel ID
initialModel = TodoModel $ coerce @[Todo ID] $ [Todo ("Get tea" ^. upwards) (False ^. upwards)]

-- Render View

renderView :: TodoModel (DynamicF Gtk) -> Markup Gtk Root AppEvent
renderView model =
  let todos' = todos model ^. deeper % mapping #children
      renderedTodos = dynamicMarkup todos' $ \ts -> column $ uncurry renderTodo <$> zip [0 ..] ts
   in column
        [ renderedTodos,
          margin Small $ button "Add Todo" (#click ?~ AddTodo),
          margin Small $ button "Print Todos" (#click ?~ PrintTodos)
        ]

renderTodo :: Int -> DynamicF Gtk (Nested Todo) -> Markup Gtk Block AppEvent
renderTodo nr todo =
  margin Small $
    let done = view deeper todo >>= view (#todoDone % deeper)
        dText = view deeper todo >>= view (#todoText % deeper)
     in row $
          margin VerySmall
            <$> [ dynamicMarkup done $ \d -> button (if d then bold "[X]" else bold "[ ]") (#click ?~ FlipChecked nr),
                  textField $ (#value .~ dText) . (#change ?~ SetText nr),
                  button "Delete" (#click ?~ DeleteTodo nr)
                ]


-- Handling app events

data AppEvent = SetText Int Text | FlipChecked Int | AddTodo | DeleteTodo Int | PrintTodos

handleEvent :: AppEvent -> ModelM TodoModel IO ()
handleEvent appEvent = case appEvent of
  SetText nr txt -> mPut (todo nr % deeper % #todoText) txt
  FlipChecked nr -> mModify (todo nr % deeper % #todoDone) not
  DeleteTodo nr -> mModify #todos (#children %~ (\c -> take nr c <> drop (nr + 1) c))
  AddTodo ->
    
      mModify
        #todos
        (#children %~ (++ [Todo ("New Todo" ^. upwards) (False ^. upwards) ^. upwards]))
  PrintTodos -> do
    todos <- mGet #todos
    liftIO $ forM_ (todos ^. #children) $ \todo -> do
      let text = todo ^. deeper % #todoText % deeper
          done = todo ^. deeper % #todoDone % deeper
      T.putStrLn $ (if done then "- [X] " else "- [ ] ") <> text
    liftIO $ T.putStrLn ""
    
  where
    todo :: Int -> AffineTraversal' (TodoModel Update) (Update (Nested Todo))
    todo nr = #todos % deeper % #children % ix nr