module Counter where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Effects exposing (Effects)

import Parts exposing (Indexed, Instance)


-- MODEL

type alias Model = Int


-- UPDATE

type Action = Increment | Decrement

update : Action -> Model -> Model
update action model =
  case action of
    Increment ->
      model + 1

    Decrement ->
      model - 1


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ button [ onClick address Decrement ] [ text "-" ]
    , div [ countStyle ] [ text (toString model) ]
    , button [ onClick address Increment ] [ text "+" ]
    ]


countStyle : Attribute
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


-- Parts expect TEA components to have effects. 
update' : Parts.Update Model Action 
update' action = 
  update action >> (\model -> (model, Effects.none))


type alias Instance container obs = 
  Parts.Instance Model container Action obs Html


instance
    : (Parts.Action (Container c) obs -> obs)
    -> Parts.Index
    -> Instance (Container c) obs
instance = 
  Parts.instance 
    view update'
    .counter (\x m -> {m | counter = x})
    0


onChange : (Action -> a) -> Action -> Maybe a
onChange f action = 
  Just (f action)
  

  
