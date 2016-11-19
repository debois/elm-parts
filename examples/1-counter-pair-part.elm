import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Dict 

import Counter
import Parts


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , subscriptions = always Sub.none
    , update = update
    , view = view
    }


-- MODEL


type alias Model =
  { counter : Counter.Indexed Counter.Model
  }


init : (Model, Cmd Msg)
init =
  ( { counter = Dict.empty }
  , Cmd.none
  )


-- UPDATE


type Msg
  = Reset
  | CounterMsg (Parts.Msg Model Msg)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      init

    CounterMsg msg_ -> 
      Parts.update msg_ model


-- VIEW


view : Model -> Html Msg
view model =
  div
    []
    [ Counter.render CounterMsg [0] model
    , Counter.render CounterMsg [1] model
    , button [ onClick Reset ] [ text "RESET" ]
    ]
