import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)

import Counters exposing (Counters)
import Help exposing (Help)
import Controls exposing (Controls)


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
  { controls : Controls
  }


init : (Model, Cmd Msg)
init =
  ( { controls = .empty Controls.instance }
  , Cmd.none
  )


-- UPDATE


type Msg
  = Reset
  | ControlMsg Controls.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Reset ->
      init

    ControlMsg msg' ->
      Controls.pass ControlMsg msg' model


-- VIEW


view : Model -> Html Msg
view model =
  div
    []
    [ Counters.render (ControlMsg << Controls.CounterMsg) [0] model.controls
    , button [ onClick Reset ] [ text "RESET" ]
    , Help.render (ControlMsg << Controls.HelpMsg) model.controls
    ]
