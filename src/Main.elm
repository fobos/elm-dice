import Graphics.Element exposing (..)
import Graphics.Input exposing (..)
import Time exposing (..)
import Random.PCG as Random exposing (..)

main : Signal Element
main =
  let
    model = Signal.foldp update init (timestamp clicks.signal)
  in
    Signal.map view model

-- Model

type alias Model = (Int, Maybe Seed)

init : Model
init = (0, Nothing)

-- Update

type Action = Initial | Click

clicks: Signal.Mailbox Action
clicks = Signal.mailbox Initial

update : (Time, Action) -> Model -> Model
update (time, action) (_, prevSeed) =
  let
    initSeed t = initialSeed (round t)

    seed = case action of
      Initial -> initSeed (time)
      Click ->
        case prevSeed of
          Just x -> x
          Nothing -> initSeed (time)

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
    flow down
      [ button ( Signal.message clicks.address Initial ) "Init seed"
      , button ( Signal.message clicks.address Click ) caption 
      ]

