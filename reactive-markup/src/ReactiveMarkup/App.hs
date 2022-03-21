module ReactiveMarkup.App where

import ReactiveMarkup.Markup
import ReactiveMarkup.Contexts.Base

data App s t e = App
  { appRender :: Dynamic t s -> Markup t Root e
  , appHandleEvent :: e -> s -> IO s
  , appInitialState :: s
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
