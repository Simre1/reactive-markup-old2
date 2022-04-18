module ReactiveMarkup.Target.Gtk.Inline where

import Control.Monad
import Data.Text as T
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Functions as Gtk
import GI.Pango.Functions ()
import ReactiveMarkup.Context
import ReactiveMarkup.Markup
import ReactiveMarkup.Target.Gtk.Base
import ReactiveMarkup.Widget
import qualified SimpleEvents as SE
import Data.Functor.Const


renderInline :: Render w Gtk Inline => w e -> Text
renderInline = getConst . render @_ @Gtk @Inline

instance Render Words Gtk Inline where
  render (Words t) = Const t

instance Render Words Gtk Block where
  render w = pangoToWidget $ renderInline w

instance Render Words Gtk Root where
  render w = pangoToWidget $ renderInline w

instance Render (Italic Gtk Inline) Gtk Inline where
  render (Italic m) = Const $ "<i>" <> getConst (renderMarkup m) <> "</i>"

instance Render (Italic Gtk Inline) Gtk Block where
  render w = pangoToWidget $ renderInline w

instance Render (Italic Gtk Inline) Gtk Root where
  render w = pangoToWidget $ renderInline w

instance Render (Bold Gtk Inline) Gtk Inline where
  render (Bold m) = Const $ "<b>" <> getConst (renderMarkup m) <> "</b>"

instance Render (Bold Gtk Inline) Gtk Block where
  render w = pangoToWidget $ renderInline w

instance Render (Bold Gtk Inline) Gtk Root where
  render w = pangoToWidget $ renderInline w

instance Render (Combine Gtk Inline) Gtk Inline where
  render (Combine a b) = Const $ getConst (renderMarkup a) <> getConst (renderMarkup b)

instance Render (Combine Gtk Inline) Gtk Block where
  render w = pangoToWidget $ renderInline w

instance Render (Combine Gtk Inline) Gtk Root where
  render w = pangoToWidget $ renderInline w

instance Render (Lift Gtk Inline) Gtk Root where
  render (Lift m) = pangoToWidget $ getConst $ renderMarkup m
