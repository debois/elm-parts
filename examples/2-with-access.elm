import Counter 
import StartApp.Simple exposing (start)
import Html exposing  (Html, div, text, br)
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

counter0 : Counter.Instance Model Action
counter0 = 
  Counter.instance PartAction [0] 


counter1 : Counter.Instance Model Action
counter1 =
  Counter.instance PartAction [1] 


view : Signal.Address Action -> Model -> Html 
view addr model = 
  div 
    []
    [ counter0.view [] addr model
    , counter1.view [] addr model
    , br [] []
    , text <| "Sum: " ++ toString (counter0.get model + counter1.get model)
    ]
  

main : Signal Html
main = 
  start
    { model = model
    , update = update
    , view = view
    }
