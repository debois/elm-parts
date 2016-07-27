import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)

import Counters exposing (Counters)


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
  { counters : Counters
  }


init : (Model, Cmd Msg)
init =
  ( { counters = .empty Counters.all }
  , Cmd.none
  )


-- UPDATE


type Msg
  = Reset
  | CounterMsg (Counters.Msg, Counters.ID)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      init

    CounterMsg msg' -> 
      Counters.pass CounterMsg msg' model


-- VIEW


view : Model -> Html Msg
view model =
  div
    []
    [ Counters.render CounterMsg [0] model
    , button [ onClick Reset ] [ text "RESET" ]
    ]
