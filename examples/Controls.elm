module Controls exposing (..)

import Parts exposing (..)
import Counters exposing (Counters)
import Help exposing (Help)



type Msg
  = CounterMsg (Counters.Msg, Counters.ID)
  | HelpMsg Help.Msg


update : Msg -> Controls -> (Controls, Cmd Msg)
update msg model =
  case msg of
    CounterMsg msg' -> 
      Counters.pass CounterMsg msg' model

    HelpMsg msg' -> 
      Help.pass HelpMsg msg' model



type alias Controls = 
  { counters : Counters
  , help : Help
  }

type alias Container c = 
  { c | controls : Controls }

pass 
   : (Msg -> outerMsg)
  -> Msg
  -> Container c
  -> (Container c, Cmd outerMsg)
pass =
  apply1 update instance

instance : Accessors Controls (Container c) 
instance = 
  accessors1 .controls (\c i -> { c | controls = i })   
    { counters = .empty Counters.all 
    , help = .empty Help.instance
    }