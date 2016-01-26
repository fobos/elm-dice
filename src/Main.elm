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

type alias Model = (Int, Maybe Seed)

init : Model
init = (0, Nothing)

-- Update

type Clicks = Initial | Click

clicks: Signal.Mailbox Clicks
clicks = Signal.mailbox Initial

update : (Time, Clicks) -> Model -> Model
update (time, click) (_, maybeSeed) =
  let
    seed =
      case maybeSeed of
        Just x -> x
        Nothing -> initialSeed (round time)
    (newVal, nextSeed) = generate (int 1 6) seed
  in
    (newVal, Just nextSeed)

-- View

view : Model -> Element
view (counterVal, _) =
  let
    caption =
      if counterVal == 0 then
        "Throw dice!!!"
      else
        toString counterVal
  in
    button (Signal.message clicks.address Click) caption
