
import Counter
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
  { topCounter : Counter.Model
  , bottomCounter : Counter.Model
  }


init : Int -> Int -> (Model, Cmd Msg)
init top bottom =
  ( { topCounter = Counter.init top
    , bottomCounter = Counter.init bottom
    }
  , Cmd.none 
  )



-- UPDATE


type Msg
  = Reset
  | Top Counter.Msg
  | Bottom Counter.Msg


update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    Reset ->
      init 0 0

    Top msg ->
      let 
        (counter', cmd) = 
          Counter.update msg model.topCounter 
      in
        ( { model | topCounter = counter' }
        , Cmd.map Top cmd
        )

    Bottom msg ->
      let 
        (counter', cmd) = 
          Counter.update msg model.bottomCounter 
      in
        ( { model | bottomCounter = counter' }
        , Cmd.map Bottom cmd
        )


-- VIEW


view : Model -> Html Msg
view model =
  div
    []
    [ Counter.view Top model.topCounter
    , Counter.view Bottom model.bottomCounter
      -- We avoid Html.App.map because of
      -- https://github.com/elm-lang/html/issues/16
    , button [ onClick Reset ] [ text "RESET" ]
    ]
