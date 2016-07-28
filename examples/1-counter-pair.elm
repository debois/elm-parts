
import Counters
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)


main : Program Never
main =
  App.program
    { init = init 0 0 
    , update = update
    , view = view
    , subscriptions = always Sub.none
    }



-- MODEL


type alias Model =
  { topCounter : Counters.Model
  , bottomCounter : Counters.Model
  }


init : Int -> Int -> (Model, Cmd Msg)
init top bottom =
  ( { topCounter = Counters.init top
    , bottomCounter = Counters.init bottom
    }
  , Cmd.none 
  )



-- UPDATE


type Msg
  = Reset
  | Top Counters.Msg
  | Bottom Counters.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    Reset ->
      init 0 0

    Top msg ->
      let 
        (counter', cmd) = 
          Counters.update msg model.topCounter 
      in
        ( { model | topCounter = counter' }
        , Cmd.map Top cmd
        )

    Bottom msg ->
      let 
        (counter', cmd) = 
          Counters.update msg model.bottomCounter
      in
        ( { model | bottomCounter = counter' }
        , Cmd.map Bottom cmd
        )


-- VIEW


view : Model -> Html Msg
view model =
  div
    []
    [ App.map Top (Counters.view model.topCounter)
    , App.map Bottom (Counters.view model.bottomCounter)
    , button [ onClick Reset ] [ text "RESET" ]
    ]
