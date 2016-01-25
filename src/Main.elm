import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Time exposing (..)
import Random.PCG as Random exposing (..)

main : Signal Element
main =
  let
    model = Signal.foldp update init (timestamp  clicks.signal)
  in
    Signal.map view model

-- Model

type alias Model = (Int, Seed)

init : Model
init = (0, initialSeed 0)

-- Model

update : (Time, Clicks) -> Model -> Model
update (time, click) (_, seed) =
  case click of
    Initial ->
      (0, initialSeed (round time))
    Click ->
      generate (int 1 6) seed

-- View

type Clicks = Initial | Click

clicks: Signal.Mailbox Clicks
clicks = Signal.mailbox Initial

view : Model -> Element
view (counterVal, _) =
  button (Signal.message clicks.address Click) (toString counterVal)
