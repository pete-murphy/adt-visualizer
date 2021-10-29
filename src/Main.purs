module Main where

import Prelude

import App as App
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception as Exception
import React.Basic.DOM as R
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

main :: Effect Unit
main = do
  maybeBody <- HTMLDocument.body =<< Window.document =<< HTML.window
  case maybeBody of
    Nothing -> Exception.throw "Could not find body"
    Just body -> do
      app <- App.mkApp
      R.render (app unit) (HTMLElement.toElement body)

