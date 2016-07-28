module Help exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Parts exposing (..)


-- MODEL


type alias Model 
  = Bool


init : Model
init =
  False


-- UPDATE


type Msg
  = Toogle


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Toogle ->
      (not model, Cmd.none)


-- VIEW


view : Model -> Html Msg
view model =
  div 
    []
    [ div [] [ button [ onClick Toogle ] [ text "Help" ] ]
    , div [ countStyle ] <| if model then [ text "You need help with this? Seriously?" ] else []
    ]


countStyle : Attribute msg
countStyle =
  style
    [ ("font-size", "16px")
    , ("font-family", "sans-serif")
    , ("display", "inline-block")
    , ("text-align", "center")
    , ("padding", "10px")
    , ("background-color", "beige")
    ]



-- Parts

type alias Help = 
  Model

type alias Container c = 
  { c | help : Help }

pass 
   : (Msg -> outerMsg)
  -> Msg
  -> Container c
  -> (Container c, Cmd outerMsg)
pass =
  apply1 update instance

render 
   : (Msg -> outerMsg)
  -> Container c
  -> Html outerMsg
render =
  create1 view instance

instance : Accessors Model (Container c) 
instance =
  accessors1 .help (\c i -> { c | help = i })  init