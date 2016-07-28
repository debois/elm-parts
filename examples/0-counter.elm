
import Counters
import Help
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)


main : Program Never
main =
  App.program
    { init = init 0 
    , update = update
    , view = view
    , subscriptions = always Sub.none
    }



-- MODEL


type alias Model =
  { counters : Counters.Model
  , help : Help.Model
  }


init : Int -> (Model, Cmd Msg)
init x =
  ( { counters = Counters.init x 
    , help = Help.init
    }
  , Cmd.none 
  )



-- UPDATE


type Msg
  = Reset
  | CounterMsg Counters.Msg
  | HelpMsg Help.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    Reset ->
      init 0 

    CounterMsg msg ->
      let 
        (counter', cmd) = 
          Counters.update msg model.counters
      in
        ( { model | counters = counter' }
        , Cmd.map CounterMsg cmd
        )
    
    HelpMsg msg ->
      let 
        (help', cmd) = 
          Help.update msg model.help
      in
        ( { model | help = help' }
        , Cmd.map HelpMsg cmd
        )

-- VIEW


view : Model -> Html Msg
view model =
  div
    []
    [ App.map CounterMsg (Counters.view model.counters)
    , button [ onClick Reset ] [ text "RESET" ]
    , App.map HelpMsg (Help.view model.help)
    ]
