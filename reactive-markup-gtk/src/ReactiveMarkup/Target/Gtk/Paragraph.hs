module ReactiveMarkup.Target.Gtk.Paragraph where

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


renderParagraph :: Render w Gtk Paragraph => w e -> Text
renderParagraph = getConst . render @_ @Gtk @Paragraph

instance Render Words Gtk Paragraph where
  render (Words t) = Const t

instance Render Words Gtk Common where
  render w = pangoToWidget $ renderParagraph w

instance Render Words Gtk Root where
  render w = pangoToWidget $ renderParagraph w

instance Render (Italic Gtk Paragraph) Gtk Paragraph where
  render (Italic m) = Const $ "<i>" <> getConst (renderMarkup m) <> "</i>"

instance Render (Italic Gtk Paragraph) Gtk Common where
  render w = pangoToWidget $ renderParagraph w

instance Render (Italic Gtk Paragraph) Gtk Root where
  render w = pangoToWidget $ renderParagraph w

instance Render (Bold Gtk Paragraph) Gtk Paragraph where
  render (Bold m) = Const $ "<b>" <> getConst (renderMarkup m) <> "</b>"

instance Render (Bold Gtk Paragraph) Gtk Common where
  render w = pangoToWidget $ renderParagraph w

instance Render (Bold Gtk Paragraph) Gtk Root where
  render w = pangoToWidget $ renderParagraph w

instance Render (Combine Gtk Paragraph) Gtk Paragraph where
  render (Combine a b) = Const $ getConst (renderMarkup a) <> getConst (renderMarkup b)

instance Render (Combine Gtk Paragraph) Gtk Common where
  render w = pangoToWidget $ renderParagraph w

instance Render (Combine Gtk Paragraph) Gtk Root where
  render w = pangoToWidget $ renderParagraph w

instance Render (Lift Gtk Paragraph) Gtk Root where
  render (Lift m) = pangoToWidget $ getConst $ renderMarkup m
