import Counter 
import StartApp exposing (start)
import Task 
import Effects exposing (Never)
import Html exposing  (Html, div, button, text)
import Html.Events exposing (onClick)
import Dict

import Parts exposing (Indexed)


type alias Model = 
  { counter : Indexed Counter.Model
  , numberOfCounters : Int
  , latest : String
  }


model : Model 
model = 
  { counter = Dict.empty
  , numberOfCounters = 0
  , latest = "No actions yet"
  }


type Action 
  = PartAction (Parts.Action Model Action)
  | AddCounter
  | RemoveCounter
  | Latest String


nofx : Model -> (Model, Effects.Effects Action)
nofx model = (model, Effects.none)


update : Action -> Model -> (Model, Effects.Effects Action)
update action model = 
  case action of 
    PartAction action' -> 
      Parts.update PartAction action' model

    RemoveCounter -> 
      nofx { model | numberOfCounters = max 0 (model.numberOfCounters - 1) }

    AddCounter -> 
      let 
        next = model.numberOfCounters + 1
      in
        { model | numberOfCounters = next }
        |> (counter [next]).set 0
        |> nofx

    Latest str -> 
      nofx { model | latest = str }


-- VIEW

counter : Parts.Index -> Counter.Instance Model Action
counter = 
  Counter.instance PartAction 


view : Signal.Address Action -> Model -> Html 
view addr model = 
  div 
    []
    (  button [ onClick addr AddCounter ] [ text "Add" ]
    :: button [ onClick addr RemoveCounter ] [ text "Remove" ]
    :: text ("  (" ++ model.latest ++ ")")
    :: ([1..model.numberOfCounters] |> List.map (\id -> 
        (counter [id]).view 
          [ Counter.onChange (\action -> 
              "Last counter action: " ++ toString action ++ " #" ++ toString id
                |> Latest)
          ] 
          addr 
          model))
    )
  

app : StartApp.App Model
app =
  start { 
    init = (model, Effects.none), 
    view = view, 
    update = update, 
    inputs = [] 
  }


main : Signal Html
main =
    app.html


port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks


