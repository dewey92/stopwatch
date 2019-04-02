module UseInput (useInput) where

import Prelude
import Data.Maybe (fromMaybe)
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (EventHandler, handler)
import React.Basic.Hooks (Hook, UseState, useState, (/\))
import React.Basic.Hooks as React

useInput
  :: String
  -> Hook
       (UseState String)
       { value :: String, onChange :: EventHandler }
useInput initialValue = React.do
  value /\ setValue <- useState initialValue

  pure
    { onChange:
        handler targetValue
          \v -> setValue \_ -> fromMaybe "" v
    , value: value
    }
