module Counter exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Parts exposing (..)


-- MODEL


type alias Model 
  = Int


init : Int -> Model
init count =
  count


-- UPDATE


type Msg
  = Increment
  | Decrement


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Increment ->
      (model + 1, Cmd.none)

    Decrement ->
      (model - 1, Cmd.none)


-- VIEW


view : Model -> Html Msg
view model =
  div 
    []
    [ button [ onClick Decrement ] [ text "-" ] 
    , div [ countStyle ] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ] 
    ]


countStyle : Attribute msg
countStyle =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")
    ]


-- Parts

type alias ID =
  Index

type alias Counters = 
  Indexed Model

type alias Container c = 
  { c | counters : Counters }

pass 
   : ((Msg, Index) -> outerMsg)
  -> (Msg, Index)
  -> Container c
  -> (Container c, Cmd outerMsg)
pass =
  apply update find

render 
   : ((Msg, Index) -> outerMsg) 
  -> Index
  -> Container c
  -> Html outerMsg
render =
  create view find

all : Collection Model (Container c)
all = 
  collection .counters (\container collection -> 
                         { container | counters = collection }) 

find : Index -> Accessors Model (Container c) 
find = 
  accessors all (init 0)