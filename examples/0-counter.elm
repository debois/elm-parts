
import Counter
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
  { counter : Counter.Model
  }


init : Int -> (Model, Cmd Msg)
init x =
  ( { counter = Counter.init x }
  , Cmd.none 
  )



-- UPDATE


type Msg
  = Reset
  | CounterMsg Counter.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    Reset ->
      init 0 

    CounterMsg msg ->
      let 
        (counter', cmd) = 
          Counter.update msg model.counter 
      in
        ( { model | counter = counter' }
        , Cmd.map CounterMsg cmd
        )

-- VIEW


view : Model -> Html Msg
view model =
  div
    []
    [ Counter.view CounterMsg model.counter
      -- We avoid Html.App.map because of
      -- https://github.com/elm-lang/html/issues/16
    , button [ onClick Reset ] [ text "RESET" ]
    ]
