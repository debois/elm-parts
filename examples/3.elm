import Counter 
import StartApp.Simple exposing (start)
import Html exposing  (Html, div, button, text)
import Html.Events exposing (onClick)
import Dict

import Parts exposing (Indexed)


type alias Model = 
  { counter : Indexed Counter.Model
  , numberOfCounters : Int
  }


model : Model 
model = 
  { counter = Dict.empty
  , numberOfCounters = 0
  }


type Action 
  = PartAction (Parts.Action Model Action)
  | AddCounter
  | RemoveCounter


update : Action -> Model -> Model
update action model = 
  case action of 
    PartAction action' -> 
      Parts.update PartAction action' model
        |> \(model', _) -> model'

    RemoveCounter -> 
      { model | numberOfCounters = max 0 (model.numberOfCounters - 1) }

    AddCounter -> 
      let 
        next = model.numberOfCounters + 1
      in
        { model | numberOfCounters = next }
        |> (counter [next]).set 0


-- VIEW

counter : Parts.Index -> Counter.Part Model Action
counter id = 
  Counter.part id PartAction 0


view : Signal.Address Action -> Model -> Html 
view addr model = 
  div 
    []
    (  button [ onClick addr AddCounter ] [ text "Add" ]
    :: button [ onClick addr RemoveCounter ] [ text "Remove" ]
    :: ([1..model.numberOfCounters] |> List.map (\id -> (counter [id]).view [] addr model))
    )
  

main : Signal Html
main = 
  start
    { model = model
    , update = update
    , view = view
    }
