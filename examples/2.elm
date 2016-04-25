import Counter 
import StartApp.Simple exposing (start)
import Html exposing  (Html, div)
import Dict

import Parts exposing (Indexed)


type alias Model = 
  { counter : Indexed Counter.Model
  }


model : Model 
model = 
  { counter = Dict.empty
  }


type Action = 
  PartAction (Parts.Action Model Action)


update : Action -> Model -> Model
update action model = 
  case action of 
    PartAction action' -> 
      Parts.update PartAction action' model
        |> \(model', _) -> model'


-- VIEW

counter0 : Counter.Part Model Action
counter0 = 
  Counter.part [0] PartAction 0


counter1 : Counter.Part Model Action
counter1 =
  Counter.part [1] PartAction 0 


view : Signal.Address Action -> Model -> Html 
view addr model = 
  div 
    []
    [ counter0.view [] addr model
    , counter1.view [] addr model
    ]
  

main : Signal Html
main = 
  start
    { model = model
    , update = update
    , view = view
    }
