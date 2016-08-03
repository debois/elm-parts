import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Dict 

import Counter
import Parts exposing (Indexed)


main : Program Never
main =
  App.program
    { init = (init, Cmd.none)
    , subscriptions = always Sub.none
    , update = update
    , view = view
    }


-- MODEL


type alias Model =
  { counter : Indexed Counter.Model 
  , first : Int
  , last : Int
  }


init : Model
init =
  { counter = Dict.empty
  , first = 0
  , last = -1
  }


-- UPDATE


type Msg
  = Insert
  | Remove
  | CounterMsg (Parts.Msg Model Msg)


reset : Int -> Model -> Model
reset k model = 
  .reset (Counter.find [k]) model


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Insert -> 
      ( { model | last = model.last + 1 } |> reset (model.last + 1)
      , Cmd.none
      )

    Remove -> 
      ( { model | first = model.first + 1 } |> reset model.first
      , Cmd.none
      )

    CounterMsg msg' -> 
      Parts.update msg' model


-- VIEW


view : Model -> Html Msg
view model =
 let
    remove =
      button [ onClick Remove ] [ text "Remove" ]

    insert =
      button [ onClick Insert ] [ text "Add" ]

    counters =
      [model.first .. model.last]
        |> List.map 
            (\idx -> Counter.render CounterMsg [idx] model) 
  in
    div [] ([remove, insert] ++ counters)
