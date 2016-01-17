import Graphics.Element exposing (..)
import Graphics.Input exposing (button)
import Random.PCG as Random exposing (..)


-- Random generator
diceNumGenerator : Generator Int
diceNumGenerator = int 1 6

throwDice : Seed -> (Int, Seed)
throwDice seed =
  generate diceNumGenerator seed

-- TODO: генерировать seed на основе уникального значения каждой сессии
seed0 : Seed
seed0 = initialSeed2 227852860 1498709020

-- Model
type alias Dice = (Int, Seed)

initDice : Dice
initDice = (0, seed0)

diceVal : Dice -> String
diceVal dice =
  let val = fst dice
  in if val == 0 then "Throw Dice!!!" else toString val

-- Signals
pressed : Signal.Mailbox Dice
pressed = Signal.mailbox initDice


onClick : Dice -> Signal.Message
onClick prevDice =
  let seed = snd prevDice
      rnd = throwDice seed
  in Signal.message pressed.address rnd

main : Signal Element
main =
  Signal.map drawButton pressed.signal

drawButton : Dice -> Element
drawButton dice =
  button (onClick dice) (diceVal dice)
