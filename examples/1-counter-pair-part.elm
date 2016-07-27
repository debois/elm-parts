import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Dict 

import Counter


main : Program Never
main =
  App.program
    { init = init
    , subscriptions = always Sub.none
    , update = update
    , view = view
    }


-- MODEL


type alias Model =
  { counters : Counter.Counters
  }


init : (Model, Cmd Msg)
init =
  ( { counters = Dict.empty }
  , Cmd.none
  )


-- UPDATE


type Msg
  = Reset
  | CounterMsg (Counter.Msg, Counter.ID)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      init

    CounterMsg msg' -> 
      Counter.pass CounterMsg msg' model


-- VIEW


view : Model -> Html Msg
view model =
  div
    []
    [ Counter.render CounterMsg [0] model
    , Counter.render CounterMsg [1] model
    , button [ onClick Reset ] [ text "RESET" ]
    ]
