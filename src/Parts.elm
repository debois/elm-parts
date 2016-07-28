module Parts exposing 
  ( Update, View, collection, Collection
  , Index, Indexed, accessors, Accessors
  , Update', create, apply, apply'
  , accessors1, create1, apply1, apply1'
  )

{-| 

Given a TEA component with model type `model` and message type `msg`, we construct
a variant component which knows how to extract its model from a container model
`container` and produces generic messages `Msg c`.

## Indexed model embeddings
@docs Index, Indexed

# Elm Architecture types
@docs Update, View

# Model embeddings
@docs accessors, accessors1, Accessors, collection, Collection

# Construction of viewable compononts
@docs create, create1

# Construction of message passing function
@docs apply, apply', apply1, apply1'

# Design

We recommend you define the following for your component:
(here for the counter component in the examples, do rename all counter-y names.)
```
type alias ID =                    -- Optional, this allows your users to avoid
  Index                            -- the explicit import of Parts

type alias Counters =              -- same here
  Indexed Model

type alias Container c =           -- A container. Please notice,
  { c | counters : Counters }      -- that your users' Model needs an unused
                                   -- field named `counters`.
pass 
   : ((Msg, Index) -> outerMsg)    -- A wrapping Msg provided by your users
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
  collection .counters (\x y -> { y | counters = x }) 

find : Index -> Accessors Model (Container c)
find =
  accessors all (init 0)
~~~

# Lazyness

Recall that `Html.Lazy` avoids re-computing views when the model doesn't change
across updates. However, "doesn't change" does not mean `model == model'` but rather
the stricter `model === model'` (in Javascript terms). That is, the old and new model
must not only be structurally the same, they must be literally the same
data-structure in memory.  

Parts generally do not achieve referential equality of no-op updates, since we
are wrapping updates conceptually like this: 

    let (submodel, submsgs) = SubComponent.update msg model.submodel 
        model' = { model | submodel = submodel }
    in 
        ...
In the second line, even if `submodel == model.submodel` and so `model ==
model'`, we won't have (in Javascript terms) `model === model'`. 

If you need lazy and thus referential equality of no-op updates, use 
`apply'` below instead of the regular `apply`. This function requires that your base `update`
function has the type of `Update'`, that is, wraps the resulting model in 
`Maybe` and explicitly signals a no-op by returning a `Nothing` model. 

@docs Update', apply'

-}

import Platform.Cmd exposing (Cmd)
import Html exposing (Html)
import Html.App as App
import Dict exposing (Dict)


{-| Type of indices. An index is a list of `Int` rather than just an `Int` to 
support nested dynamically constructed elements: Use indices `[0]`, `[1]`, ...
for statically known top-level components, then use `[0,0]`, `[0,1]`, ...
for a dynamically generated list of components. 
-}
type alias Index 
  = List Int
 

{-| Indexed families of things.
-}
type alias Indexed a = 
  Dict Index a 


{-| Standard TEA update function type. 
-}
type alias Update model msg = 
  msg -> model -> (model, Cmd msg)


{-| Standard TEA view function type. 
-}
type alias View model a = 
  model -> Html a


{-| TEA update function with explicit no-op. You should have:

    fst (update msg model) == Nothing       -- No change to model
    fst (update msg model) == Just model'   -- Change to model'

-}
type alias Update' model msg = 
  msg -> model -> (Maybe model, Cmd msg)


{-| For components where consumers do care about the model of the 
component, use the `accessors` function below to generate suitable, 
well, accessors.
-}
type alias Accessors model container = 
  { empty : model
  , get : container -> model
  , set : container -> model -> container
  , map : (model -> model) -> container -> container
  , reset : container -> container
  }


{-| A collection abstracting over all the instances of a component.
If you want to apply an action to all the instances of a component,
use this data structure.
-}
type alias Collection model container =
  { empty : Indexed model
  , get : container -> Indexed model
  , set : container -> Indexed model -> container
  , map : (Index -> model -> model) -> container -> container
  , reset : container -> container
  }



{-| Construct a collection:

    all : Collection Model (Container c)
    all = 
      collection .myField (\x y -> { y | myField = x }) 

-}
collection
  : (container -> (Indexed model))
 -> (container -> (Indexed model) -> container)
 -> Collection model container
collection get set =
  { empty = Dict.empty
  , get = get
  , set = set
  , map = \f c -> get c |> Dict.map f |> set c 
  , reset = flip set Dict.empty
  }


{-| Generate accessors:

    find = 
      accessors all init

-}
accessors 
  : Collection model container
 -> model
 -> Index
 -> Accessors model container
accessors collection model0 idx =
  let
    get =
      Maybe.withDefault model0 << Dict.get idx << collection.get

    set container model =
      collection.set container <| Dict.insert idx model <| collection.get container
  in
    { empty = model0
    , get = get
    , set = set
    , map = \f c -> get c |> f |> set c
    , reset = \c -> collection.get c 
                 |> Dict.remove idx 
                 |> collection.set c
    }


{-| Generate accessors for a component, that has only one instance.

    instance = 
      accessors1 .myField (\x y -> { y | myField = x }) init

-}
accessors1
  : (container -> model)
 -> (container -> model -> container)
 -> model
 -> Accessors model container
accessors1 get set model0 =
  { empty = model0
  , get = get
  , set = set
  , map = \f c -> get c |> f |> set c
  , reset = flip set model0
  }


{-| Create a viewable component.

    For your component:
    render =
      create view find

    In your main view:
    Component.render ComponentMsg [0] model
-}
create
   : View model innerMsg 
  -> (Index -> Accessors model container)
  -> ((innerMsg, Index) -> outerMsg)
  -> Index
  -> container
  -> Html outerMsg
create view access wrapper idx container = 
  App.map (\msg -> wrapper (msg, idx)) (view ((access idx).get container))


{-| Create a viewable component, that has only one instance.

    For your component:
    render =
      create1 view instance

    In your main view:
    Component.render ComponentMsg model
-}
create1
   : View model innerMsg 
  -> Accessors model container
  -> (innerMsg -> outerMsg)
  -> container
  -> Html outerMsg
create1 view instance wrapper = 
  App.map wrapper << view << instance.get


{-| Create a function, to pass messages down to your component 

    For your component:
    pass =
      apply update find

    In your main update:
    ComponentMsg msg' -> 
      Component.pass ComponentMsg msg' model
-}
apply
   : Update model innerMsg
  -> (Index -> Accessors model container)
  -> ((innerMsg, Index) -> outerMsg)
  -> (innerMsg, Index)
  -> container
  -> (container, Cmd outerMsg)
apply update = apply' (\msg -> map1st Just << update msg)


{-| `apply` for components, which `update` function returns the model as a Maybe,
    depending on whether it changed or not. See the comment on laziness above.
-}
apply'
   : Update' model innerMsg
  -> (Index -> Accessors model container)
  -> ((innerMsg, Index) -> outerMsg)
  -> (innerMsg, Index)
  -> container
  -> (container, Cmd outerMsg)
apply' update access wrapper (msg, idx) container =
  let 
    item = access idx
  in
    update msg (item.get container)
      |> map1st (Maybe.map (item.set container) >> Maybe.withDefault container)
      |> map2nd (Cmd.map (\msg' -> wrapper (msg', idx)))


{-| Create a function, to pass messages down to your component,
 for a component, that has only one instance.

    For your component:
    pass =
      apply1 update instance

    In your main update:
    ComponentMsg msg' -> 
      Component.pass ComponentMsg msg' model
-}
apply1
   : Update model innerMsg
  -> Accessors model container
  -> (innerMsg -> outerMsg)
  -> innerMsg
  -> container
  -> (container, Cmd outerMsg)
apply1 update = apply1' (\msg -> map1st Just << update msg)


{-| `apply1` for components, which `update` function returns the model as a Maybe,
    depending on whether it changed or not. See the comment on laziness above.
-}
apply1'
   : Update' model innerMsg
  -> Accessors model container
  -> (innerMsg -> outerMsg)
  -> innerMsg
  -> container
  -> (container, Cmd outerMsg)
apply1' update instance wrapper msg container =
  update msg (instance.get container)
    |> map1st (Maybe.map (instance.set container) >> Maybe.withDefault container)
    |> map2nd (Cmd.map wrapper)


-- Helpers

map1st : (a -> c) -> (a,b) -> (c,b)
map1st f (x,y) = (f x, y)


map2nd : (b -> c) -> (a,b) -> (a,c)
map2nd f (x,y) = (x, f y)