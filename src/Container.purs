module Container (stopwatch) where

import Prelude
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable (null, toNullable)
import Effect.Timer (clearInterval, setInterval)
import Global (readFloat)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (CreateComponent, component, (/\))
import React.Basic.Hooks as React
import UseInput (useInput)

-- | Milliseconds type
-- The only reason why I exclude it from `TimeUnit` is because
-- I just want to play around the concept of `Newtype`
-- and `derive newtype instance`
newtype Milliseconds = Milliseconds Number

instance showMs :: Show Milliseconds where
  show (Milliseconds ms) = show ms <> "ms"
derive instance newtypeMs :: Newtype Milliseconds _
derive newtype instance semiringMs :: Semiring Milliseconds
derive newtype instance ringMs :: Ring Milliseconds
derive newtype instance eqMs :: Eq Milliseconds
derive newtype instance ordMs :: Ord Milliseconds

class ToMilliseconds t where
  toMilliseconds :: t -> Milliseconds

-- | Percent type
newtype Percent = Percent Number
instance showPercent :: Show Percent where
  show (Percent a) = show a <> "%"

-- | Time Unit type
data TimeUnit = Second Number | Minute Number | Hour Number
instance toMsTimeUnit :: ToMilliseconds TimeUnit where
  toMilliseconds (Second s) = Milliseconds $ s * 1000.0
  toMilliseconds (Minute m) = Milliseconds $ m * 1000.0 * 60.0
  toMilliseconds (Hour h) = Milliseconds $ h * 1000.0 * 60.0 * 60.0

timeUnitConst :: { seconds :: String
, minutes :: String
, hours :: String
}
timeUnitConst =
  { seconds: "seconds"
  , minutes: "minutes"
  , hours: "hours"
  }

toTimeUnit :: String -> String -> TimeUnit
toTimeUnit tAmount tUnit
  | tUnit == timeUnitConst.seconds = Second $ readFloat tAmount
  | tUnit == timeUnitConst.minutes = Minute $ readFloat tAmount
  | tUnit == timeUnitConst.hours = Hour $ readFloat tAmount
  | otherwise = Second 0.0

-- | Update state every 100ms
intervalTime :: Milliseconds
intervalTime = Milliseconds 100.0

msToStyle :: TimeUnit -> Milliseconds -> R.CSS
msToStyle original (Milliseconds remaining) = percentToCss msToPercent
  where
    percentToCss p = R.css { "--percent": show p }
    original' = unwrap $ toMilliseconds original
    msToPercent =
      let percentage = (original' - remaining) / original' * 100.0
      in Percent percentage

-- | Reducer actions
data Actions = Start TimeUnit | Update | Pause | Resume | Reset
data Progress = Running | Paused | Stopped
derive instance eqProgress :: Eq Progress

-- | State
type State =
  { progress :: Progress
  , msRemaining :: Milliseconds
  }

initialState :: State
initialState =
  { progress: Stopped
  , msRemaining: zero
  }

reducer :: State -> Actions -> State
reducer state = case _ of
  (Start timeUnit) ->
    if state.progress == Paused || state.progress == Stopped
    then state { progress = Running, msRemaining = toMilliseconds timeUnit }
    else state { msRemaining = toMilliseconds timeUnit }
  Update ->
    let newRemaining = state.msRemaining - intervalTime
    in if (newRemaining <= zero)
          then reducer state Reset
          else state { msRemaining = newRemaining }
  Pause -> state { progress = Paused }
  Resume -> state { progress = Running }
  Reset -> initialState

-- | THE component
stopwatch :: CreateComponent {}
stopwatch = component "Stopwatch" \props -> React.do
  state /\ dispatch <- React.useReducer initialState reducer
  timeAmount <- useInput "0"
  timeUnit <- useInput "seconds"
  intervalIdRef <- React.useRef null

  let readFromInput = toTimeUnit timeAmount.value timeUnit.value
  let stopStopwatch = do
        ref' <- React.readRefMaybe intervalIdRef
        case ref' of
          (Just r) -> clearInterval r
          (Nothing) -> pure mempty

  React.useEffect state.progress do
    let intervalTimeInInt = fromMaybe 0 (fromNumber $ unwrap intervalTime)
    if state.progress == Running
       then do
            intervalId <- setInterval intervalTimeInInt $ dispatch Update
            React.writeRef intervalIdRef (toNullable $ Just intervalId)
       else stopStopwatch
    pure $ stopStopwatch

  pure $
    R.div_
    [ R.h1_ [ R.text "Stopwatch for your 5-year old kid" ]
    , R.div
      { className: "container"
      , children:
        [ R.div { className: "circle circle--green" }
        , R.div
            { className: "circle circle--red"
            , style: if state.progress /= Stopped
                        then msToStyle readFromInput state.msRemaining
                        else R.css {}
            }
        , R.div
            { className: "control"
            , children:
              [ R.input
                  { type: "number"
                  , value: timeAmount.value
                  , onChange: timeAmount.onChange
                  }
              , R.select
                  { name: "unit"
                  , value: timeUnit.value
                  , onChange: timeUnit.onChange
                  , children:
                    [ R.option { value: timeUnitConst.seconds, children: [ R.text timeUnitConst.seconds ] }
                    , R.option { value: timeUnitConst.minutes, children: [ R.text timeUnitConst.minutes ] }
                    , R.option { value: timeUnitConst.hours, children: [ R.text timeUnitConst.hours ] }
                    ]
                  }
              ]
            }
        , R.div
            { className: "control",
              children:
              [ R.button
                  { onClick: handler_ $ dispatch $ Start readFromInput
                  , children: [ R.text "Start" ]
                  }
              , case state.progress of
                    Stopped -> mempty
                    Running -> R.button { onClick: handler_ $ dispatch Pause, children: [ R.text "Pause" ] }
                    Paused -> R.button { onClick: handler_ $ dispatch Resume, children: [ R.text "Resume" ] }
              , R.button { onClick: handler_ $ dispatch Reset, children: [R.text "Reset"] }
              ]
            }
        ]
      }
    ]
