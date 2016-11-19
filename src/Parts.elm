module Parts exposing 
  ( Update, View
  , Get, Set, embedView, embedUpdate
  , Index, Indexed, indexed
  , Msg
  , partial, update, update_, create, create1, accessors, Accessors
  , generalize, pack, pack1
  )

{-| 

Given a TEA component with model type `model` and message type `msg`, we construct
a variant component which knows how to extract its model from a c model
`c` and produces generic messages `Msg c`. The consuming component is assumed
to have message type `obs` (for "observation"). 

# Lazyness

Recall that `Html.Lazy` avoids re-computing views when the model doesn't change
across updates. However, "doesn't change" does not mean `model == model_` but rather
the stricter `model === model_` (in Javascript terms). That is, the old and new model
must not only be structurally the same, they must be literally the same
data-structure in memory.  

Parts generally do not achieve referential equality of no-op updates, since we
are wrapping updates conceptually like this: 

    let (submodel, submsgs) = SubComponent.update msg model.submodel 
        model_ = { model | submodel = submodel }
    in 
        ...
In the second line, even if `submodel == model.submodel` and so `model ==
model_`, we won't have (in Javascript terms) `model === model_`. 

For this reason, the result of `update` functions used in parts should be
`Maybe (model, Cmd msg)` rather than the usual `(model, Cmd msg)`; the 
`Nothing` case signifies a no-op. 

# Communicating to the parent component

Because parts wrap messages in an opaque type, the parent component loses the
ability to inspect and maybe react to messages of the part. We recover this 
ability by requiring the `update` function to take as parameter a lifting 
function which lifts the parts messages to that of its parent. 

@docs Update, View

# Model embeddings 
@docs Get, Set, embedView, embedUpdate
@docs accessors, Accessors

## Indexed model embeddings
@docs Index, Indexed, indexed

# Message embeddings
@docs Msg, update, update_, partial

# Part construction
@docs create, create1, generalize, pack, pack1
-}

import Dict exposing (Dict)


-- TYPES


{-| Update functions. 

TEA update function with explicit message lifting and no-op. You should have:

    Tuple.first (update f msg model) == Nothing       -- No change to model
    Tuple.first (update f msg model) == Just model_   -- Change to model_
-}
type alias Update model msg obs = 
  (msg -> obs) -> msg -> model -> Maybe (model, Cmd obs)


{-| Standard TEA view function type. 
-}
type alias View model a = 
  model -> a


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
embedUpdate
    : (d -> e)
    -> (a -> d -> c)
    -> (f -> g -> e -> ( Maybe a, b ))
    -> f
    -> g
    -> d
    -> ( Maybe c, b )
embedUpdate get set update = 
  \f msg c -> 
     update f msg (get c) 
       |> map1st (Maybe.map <| flip set c)


-- INDEXED EMBEDDINGS

 
{-| Type of indices. An index has to be `comparable`

For example:
An index can be a list of `Int` rather than just an `Int` to
support nested dynamically constructed elements: Use indices `[0]`, `[1]`, ...
for statically known top-level components, then use `[0,0]`, `[0,1]`, ...
for a dynamically generated list of components.
-}
type alias Index comparable = 
  comparable


{-| Indexed families of things.
-}
type alias Indexed comparable a
  = Dict (Index comparable) a


{-| Fix a getter and setter for an `Indexed comparable model` to a particular `Index comparable`.
-}
indexed : 
    Get (Indexed comparable model) c
 -> Set (Indexed comparable model) c
 -> model
 -> (Index comparable -> Get model c, Index comparable -> Set model c)
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
type Msg c obs = 
  Msg (c -> (Maybe c, Cmd obs))


{-| Generic update function for `Msg`. 
-}
update : Msg c obs -> c -> (c, Cmd obs)  
update (Msg f) c = 
  f c |> map1st (Maybe.withDefault c)
  

{-| Generic update function for `Msg`, explicit no-op 
-}
update_ : Msg c obs -> c -> (Maybe c, Cmd obs)  
update_ (Msg f) c = 
  f c 
  


-- PARTS


{-| Partially apply an `Update` function to a `msg`, producing
a generic Msg.
-}
partial
    : (Msg b obs -> c)
    -> ((a -> c) -> a -> b -> ( Maybe b, Cmd obs ))
    -> a
    -> Msg b obs
partial fwd upd msg = 
  Msg (\c -> 
    upd (partial fwd upd >> fwd) msg c)


{-| Pack up a an indexed component message `msg` in an `obs`.
-}
pack
    : ((a -> c) -> a -> b -> ( Maybe b, Cmd obs ))
    -> Get (Indexed comparable b) c1
    -> Set (Indexed comparable b) c1
    -> b
    -> (Msg c1 obs -> c)
    -> Index comparable
    -> a
    -> c
pack update get0 set0 model0 fwd = 
  let
    (get, set) = 
      indexed get0 set0 model0 
  in
    \idx -> 
      partial fwd (embedUpdate (get idx) (set idx) update) >> fwd


{-| Pack up a singleton component message `msg` in an `obs`.
-}
pack1
    : ((b -> c) -> b -> d -> ( Maybe a, Cmd obs ))
    -> (c1 -> d)
    -> (a -> c1 -> c1)
    -> (Msg c1 obs -> c)
    -> b
    -> c
pack1 update get set fwd = 
  partial fwd (embedUpdate get set update) >> fwd



{-| From `update` and `view` functions, produce a `view` function which (a) 
fetches its model from a `c` model, and (b) dispatches generic `Msg`
messages. 

Its instructive to compare the types of the input `view` and `update` for a 
typical case. Notice that `create` transforms `model` -> `c` and
`Html m` -> `Html obs`.

  {- Input -}
  view : (m -> obs) -> model -> List (Attributes obs) -> List (Html obs) -> Html obs
  update : (m -> obs) -> model -> (Maybe model, Cmd obs)

  {- Output -}
  view : Index comparable -> c -> List (Attributes obs) -> List (Html obs) -> Html obs

Note that the input `view` function is assumed to take a function lifting its
messages. 
-}
create
    : ((a -> c) -> b -> d)
    -> ((a -> c) -> a -> b -> ( Maybe b, Cmd obs ))
    -> Get (Indexed comparable b) e
    -> Set (Indexed comparable b) e
    -> b
    -> (Msg e obs -> c)
    -> Index comparable
    -> e
    -> d
create view update get0 set0 model0 fwd = 
  let
    get = 
      Tuple.first (indexed get0 set0 model0)

    embeddedUpdate = 
      pack update get0 set0 model0 fwd
  in
    \idx c -> 
      (view (embeddedUpdate idx) (get idx c))


{-| Like `create`, but for components that are assumed to have only one
instance.
-}
create1
    : ((b -> c) -> View d a)
    -> ((b -> c) -> b -> d -> ( Maybe a1, Cmd obs ))
    -> (c1 -> d)
    -> (a1 -> c1 -> c1)
    -> (Msg c1 obs -> c)
    -> View c1 a
create1 view update get set fwd = 
  let
    embeddedUpdate = 
      partial fwd (embedUpdate get set update) >> fwd
  in 
    embedView get <| view embeddedUpdate


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
  : Get (Dict comparable model) c
 -> Set (Dict comparable model) c
 -> model 
 -> Index comparable
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


{-| Generalise a standard TEA `update` function to one fitting with 
parts (explicit lifter, explicit no-op). 
-}
generalize
    : (b -> c -> ( a, Cmd a1 ))
    -> (a1 -> msg)
    -> b
    -> c
    -> ( Maybe a, Cmd msg )
generalize upd f m c = 
  upd m c 
    |> map1st Just
    |> map2nd (Cmd.map f)


-- HELPERS


map1st : (a -> c) -> (a,b) -> (c,b)
map1st f (x,y) = (f x, y)


map2nd : (b -> c) -> (a,b) -> (a,c)
map2nd f (x,y) = (x, f y)


