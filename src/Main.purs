module Main where

import Prelude

import Container (stopwatch)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import React.Basic.Hooks (element)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  container <- getElementById "root" =<< (map toNonElementParentNode $ window >>= document)
  case container of
    Nothing -> throw "Container element not found."
    Just c  -> do
      app <- stopwatch
      render (element app {}) c
