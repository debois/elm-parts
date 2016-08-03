module Counter exposing (Model, init, Msg, update, view, render, find)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Parts exposing (Indexed)


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


view : (Msg -> m) -> Model -> Html m
view lift model =
  div 
    []
    [ button [ onClick (lift Decrement) ] [ text "-" ] 
    , div [ countStyle ] [ text (toString model) ]
    , button [ onClick (lift Increment) ] [ text "+" ] 
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


-- PART


type alias Container c = 
  { c | counter : Indexed Model }


set : Parts.Set (Indexed Model) (Container c) 
set x y = 
  { y | counter = x }


render : (Parts.Msg (Container c) m -> m) -> Parts.Index -> Container c -> Html m
render = 
  Parts.create view (Parts.generalize update) .counter set (init 0)


find : Parts.Index -> Parts.Accessors Model (Container c) 
find = 
  Parts.accessors .counter set (init 0)
