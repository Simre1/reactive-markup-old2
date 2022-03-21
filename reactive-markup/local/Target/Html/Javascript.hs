module ReactiveMarkup.Target.Html.Javascript where

import Data.Text
import Data.Void (Void)
import Control.Category
import Prelude hiding ((.), id)

newtype JSFunc a b = JSFunc Text

newtype JSVal a = JSVal Text

jsConst :: JSVal x -> JSFunc a x
jsConst (JSVal val) = JSFunc $ "((x) => " <> val <> ")"

jsLog :: JSFunc a Void
jsLog = JSFunc "((x) => console.log(x))"

jsAbsurd :: JSFunc Void a
jsAbsurd = JSFunc "((x) => null)"

jsApply :: JSFunc a b -> JSVal a -> JSVal b
jsApply (JSFunc f) (JSVal a) = JSVal $ f <> "(" <> a <> ")"

instance Category JSFunc where
  id = JSFunc "(x => x)"
  (JSFunc a) . (JSFunc b) = JSFunc $ "((x) => (" <> b <> "(" <> a <> "(x))))"

class ToJSVal a where
  toJSVal :: a -> JSVal a

instance ToJSVal () where
  toJSVal _ = JSVal "0"

instance ToJSVal Int where
  toJSVal = JSVal . pack . show

instance ToJSVal Void where
  toJSVal _ = JSVal "null"

instance ToJSVal Text where
  toJSVal t = JSVal $ "\"" <> t <> "\""


