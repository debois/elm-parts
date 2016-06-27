module Parts exposing 
  ( Update, View
  , Get, Set, embedView, embedUpdate
  , Index, Indexed, indexed
  , Msg
  , pack, update, create, create1, accessors, Accessors
  , Update', pack', update', create1', embedUpdate'
  )

{-| 

Given a TEA component with model type `model` and message type `msg`, we construct
a variant component which knows how to extract its model from a c model
`c` and produces generic messages `Msg c`. The consuming component is assumed
to have message type `obs` (for "observation"). 

# Elm Architecture types
@docs Update, View

# Model embeddings 
@docs Get, Set, embedView, embedUpdate
@docs accessors, Accessors

## Indexed model embeddings
@docs Index, Indexed, indexed

# Message embeddings
@docs Msg, update, pack

# Part construction
@docs create, create1

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
`update'` below instead of the regular `update`, and create your parts with 
`create1'`, `pack'` etc. These functions all require that your base `update`
function has the type of `Update'`, that is, wraps the resulting model in 
`Maybe` and explicitly signals a no-op by returning a `Nothing` model. 

@docs Update', update', create1', pack', embedUpdate'

-}

import Platform.Cmd exposing (Cmd)
import Dict exposing (Dict)


-- TYPES


{-| Standard TEA update function type. 
-}
type alias Update model msg = 
  msg -> model -> (model, Cmd msg)


{-| Standard TEA view function type. 
-}
type alias View model a = 
  model -> a


{-| TEA update function with explicit no-op. You should have:

    fst (update msg model) == Nothing       -- No change to model
    fst (update msg model) == Just model'   -- Change to model'

-}
type alias Update' model msg = 
  msg -> model -> (Maybe model, Cmd msg)


-- EMBEDDINGS


{-| Type of "getter": fetch component model `m` from c model `c`. 
-}
type alias Get model c =
  c -> model


{-| Type of "setter": update component model `m` in c `c`. 
-}
type alias Set model c = 
  model -> c -> c


{-| Lift a `view` to one which knows how to retrieve its `model` from 
a c model `c`. 
-}
embedView : Get model c -> View model a -> View c a
embedView get view = 
  get >> view 


{-| Lift an `Update` from operating on `model` to a c model `c`. 
-}
embedUpdate : 
    Get model c
 -> Set model c
 -> Update model msg
 -> Update c msg
embedUpdate get set update = 
  \msg c -> 
     update msg (get c) |> map1st (flip set c)


{-| Lift an explicit no-op `Update'` from operating on `model` to a c model `c`. 
-}
embedUpdate' :
    Get model c
 -> Set model c
 -> Update' model msg
 -> Update' c msg
embedUpdate' get set update = 
  \msg c -> 
    update msg (get c) 
      |> map1st (Maybe.map (\x -> set x c))



-- INDEXED EMBEDDINGS

 
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


{-| Fix a getter and setter for an `Indexed model` to a particular `Index`.
-}
indexed : 
    Get (Indexed model) c
 -> Set (Indexed model) c
 -> model
 -> (Index -> Get model c, Index -> Set model c)
indexed get set model0 =  
  ( \idx c -> Dict.get idx (get c) |> Maybe.withDefault model0
  , \idx model c -> set (Dict.insert idx model (get c)) c
  )
  

-- EMBEDDING MESSAGES


{-| Similar to how embeddings enable collecting models of different type
in a single model c, we collect messages in a single "master
message" type. Messages exist exclusively to be dispatched by a corresponding
`update` function; we can avoid distinguishing between different types of 
messages by dispatching not the `Msg` itself, but a partially applied update
function `update msg`. 

It's instructive to compare `Msg` to the type of `update` partially applied to 
an actual carried message `m`:

    update : m -> c -> (c, Cmd m)
    (update m) : c -> (c, Cmd m)
-}
type Msg c = 
  Msg (c -> (Maybe c, Cmd (Msg c)))


{-| Generic update function for `Msg`. 
-}
update : (Msg c -> m) -> Msg c -> c -> ( c, Cmd m )  
update fwd (Msg f) c = 
  f c 
    |> map1st (Maybe.withDefault c)
    |> map2nd (Cmd.map fwd)
  

{-| Generic explict no-op update function for `Msg`. 
-}
update' : (Msg c -> m) -> Msg c -> c -> ( Maybe c, Cmd m )  
update' fwd (Msg f) c = 
  f c 
    |> map2nd (Cmd.map fwd)
  

-- PARTS


{-| Partially apply an `Update` function to a `msg`, producing a generic Msg.
-}
pack : (Update c msg) -> msg -> Msg c 
pack upd msg = 
  Msg (\c -> 
    upd msg c 
      |> map1st Just
      |> map2nd (Cmd.map (pack upd)))


{-| Partially apply an explicit no-op `Update'` function to a `msg`, producing
a generic Msg.
-}
pack' : (a -> b -> ( Maybe b, Cmd a )) -> a -> Msg b
pack' upd msg = 
  Msg (\c -> 
    upd msg c 
      |> map2nd (Cmd.map (pack' upd)))



{-| From `update` and `view` functions, produce a `view` function which (a) 
fetches its model from a `c` model, and (b) dispatches generic `Msg`
messages. 

Its instructive to compare the types of the input `view` and `update` for a 
typical case. Notice that `create` transforms `model` -> `c` and
`Html m` -> `Html (Msg c)`. 

  {- Input -}
  view : (m -> obs) -> model -> List (Attributes m) -> List (Html m) -> Html m
  update : m -> model -> (model, Cmd m)

  {- Output -}
  type alias m' = Msg c
  view : c -> List (Attributes m') -> List (Html m') -> Html m'

Note that the input `view` function is assumed to take a function lifting its
messages. 

-}
create 
  : ((m -> obs) -> View model a)
 -> Update model m
 -> Get (Indexed model) c
 -> Set (Indexed model) c
 -> model 
 -> (Msg c -> obs)
 -> Index
 -> View c a
create view update get0 set0 model0 = 
  let
    (get, set) = 
      indexed get0 set0 model0 

    embeddedUpdate idx = 
      embedUpdate (get idx) (set idx) update
  in
    \f idx c -> 
      (view (pack (embeddedUpdate idx) >> f)) (get idx c) 


{-| Like `create`, but for components that are assumed to have only one
instance.
-}
create1
  : ((msg -> obs) -> View model a)
 -> Update model msg
 -> Get model c
 -> Set model c
 -> (Msg c -> obs)
 -> View c a

create1 view update get set = 
  let
    embeddedUpdate = 
      embedUpdate get set update
  in 
    \f -> 
      embedView get <| view (pack embeddedUpdate >> f)


{-| Like `create1`, but for explicit no-op update functions. 
-}
create1'
  : ((msg -> obs) -> View model a)
 -> Update' model msg
 -> Get model c
 -> Set model c
 -> (Msg c -> obs)
 -> View c a

create1' view upd' get set = 
  let
    embeddedUpdate = 
      embedUpdate' get set upd'
  in 
    \f -> 
      embedView get <| view (pack' embeddedUpdate >> f)


{-| For components where consumers do care about the model of the 
component, use the `accessors` function below to generate suitable, 
well, accessors.
-}
type alias Accessors model c = 
  { get : Get model c
  , set : Set model c
  , map : (model -> model) -> c -> c
  , reset : c -> c
  }


{-| Generate accessors.
-}
accessors 
  : Get (Indexed model) c
 -> Set (Indexed model) c
 -> model 
 -> Index
 -> Accessors model c

accessors get0 set0 model0 idx = 
  let
    (get, set) =
      indexed get0 set0 model0 
  in
    { get = get idx
    , set = set idx
    , map = \f c -> (get idx) c |> f |> flip (set idx) c
    , reset = \c -> get0 c |> Dict.remove idx |> (\m -> set0 m c)
    }


-- HELPERS


map1st : (a -> c) -> (a,b) -> (c,b)
map1st f (x,y) = (f x, y)


map2nd : (b -> c) -> (a,b) -> (a,c)
map2nd f (x,y) = (x, f y)


