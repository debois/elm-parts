module Parts
  ( embed, embedIndexed, Embedding, Observer
  , View, Update, Indexed
  , Part, new, new1
  , update
  , Action
  ) where

{-| 

# Elm Architecture types
@docs Update, View

# Embeddings 
@docs Indexed, Embedding, embed, embedIndexed

# Part construction
@docs Action, Part, Observer, new, new1

# Part consumption
@docs update

-}

import Effects exposing (Effects)
import Task
import Dict exposing (Dict)



-- TYPES



{-| Standard TEA update function type. 
-}
type alias Update model action = 
  action -> model -> (model, Effects action)


{-| Variant of TEA update function type, where effects may be 
lifted to a different type. 
-}
type alias Update' model action action' = 
  action -> model -> (model, Effects action')



{-| Standard TEA view function type. 
-}
type alias View model action a = 
  Signal.Address action -> model -> a



-- EMBEDDING MODELS 



{-| Indexed families of things.
-}
type alias Indexed a = 
  Dict Int a 


{-| An __embedding__ of an Elm Architecture component is a variant in which
view and update functions know how to extract and update their model 
from a larger master model. 
-}
type alias Embedding model container action a = 
  { view : View container action a
  , update : Update container action 
  , getModel : container -> model
  , setModel : model -> container -> container
  }
 

{-| Embed a component. Third and fourth arguments are a getter (extract the 
local model from the container) and a setter (update local model in the 
container). 

It is instructive to compare the types of the view and update function in 
the input and output:

     {- Input -}                    {- Output -}
     View model action a            View container action a
     Update model action            Update container action 

-}
embed : 
  View model action a ->               -- Given a view function, 
  Update model action ->               -- an update function 
  (container -> model) ->              -- a getter 
  (model -> container -> container) -> -- a setter
  Embedding model container action a   -- produce an Embedding. 

embed view update get set = 
  { view = 
      \addr model -> view addr (get model)
  , update = 
      \action model -> 
        update action (get model)
          |> map1st (flip set model)
  , getModel = get
  , setModel = set
  }


{-| We are interested in particular embeddings where components of the same
type all have their state living inside a shared `Dict`; the individual
component has a key used to look up its own state. 
-}
embedIndexed : 
  View model action a ->                       -- Given a view function, 
  Update model action ->                       -- an update function 
  (container -> Indexed model) ->              -- a getter 
  (Indexed model -> container -> container) -> -- a setter
  model ->                                     -- an initial model for this part
  Int ->                                       -- a part id (*)
  Embedding model container action a           -- ... produce a Part.

embedIndexed view update get set model0 id = 
  let 
    get' model = 
      Dict.get id (get model) |> Maybe.withDefault model0

    set' submodel model = 
      set (Dict.insert id submodel (get model)) model 
  in 
      embed view update get' set' 



-- LIFTING ACTIONS



{-| Similarly to how embeddings enable collecting models of different type
in a single model container, we need to collect actions in a single "master
action" type.  Obviously, actions need to be eventually executed by running
the corresponding update function. To avoid this master action type explicitly
representing the Action/update pairs of elm-mdl components, we represent an
action of an individual component as a partially applied update function; that
is, a function `container -> container`. E.g., the `Click` action of Button is
conceptually represented as:

    embeddedButton : Embedding Button.Model container action ...
    embeddedButton = 
      embedIndexed 
        Button.view Button.update .button {\m x -> {m|button=x} Button.model 0

    clickAction : container -> container 
    clickAction = embeddedButton.update Button.click 

When all components are embedded in the same `container` model, we 
then have a uniform update mechanism. 

We lost the ability to inspect the action when we did this, though. To be 
able to react to some actions of a component, we add to our `container -> 
container` type for actions a potential __observation__ of type `obs`. 
In practice, this observation type `obs` will be the Action of the TEA
component __hosting__ the sub-components. 

Altogether, accounting also for effects, we arrive at the following type. 
-}
type Action container obs = 
  A (container -> (container, Effects (Action container obs), Maybe obs))


{-| Type of observers, i.e., functions that take an actual action of the 
underlying TEA component to an observation.  E.g., Button has an Observer for
its `Click` action. 
-}
type alias Observer action obs = 
  action -> Maybe obs


{-| Generic update function for Action. 
-}
update : 
  (Action container obs -> obs) ->      
  Update' container (Action container obs) obs

update fwd (A f) container = 
  let 
    (container', fx, obs) = 
      f container
        |> map2 (Effects.map fwd)
  in 
    case obs of 
      Nothing -> 
        (container', fx)

      Just x -> 
        (container', Effects.batch [ fx, Effects.task (Task.succeed x) ]) 




-- INSTANCES






{-| Type of parts. A part contains a view, 
get/set/map for the inner model, and a forwarder lifting component 
actions to observations. 
-}
type alias Part model container action obs a = 
  { view : View container obs a
  , get : container -> model
  , set : model -> container -> container
  , map : (model -> model) -> container -> container
  , fwd : action -> obs 
  }


{- TEA update function variant where running the function
produces not just a new model and an effect, but also an 
observation.
-}
type alias Step model action obs =
  action -> model -> (model, Effects action, Maybe obs)
  

{- Partially apply a step function to an action, producing a generic Action.
-}
pack : (Step model action obs) -> action -> Action model obs
pack update action = 
  A (update action >> map2 (Effects.map (pack update))) 


{- Convert an update function to a step function by applying a 
function that converts the action input to the update function into
an observation.
-}
observe : Observer action obs -> Update model action -> Step model action obs
observe f update action =
  update action >> (\(model', effects) -> (model', effects, f action))


{- Return the first non-Nothing value in the list, or Nothing if no such
exists.
-}
pick : (a -> Maybe b) -> List a -> Maybe b
pick f xs = 
  case xs of 
    [] -> Nothing 
    x :: xs' -> 
      case f x of 
        Nothing -> pick f xs' 
        x -> x


{- Promote a list of Observers to a single Observer by picking, for a given
action, the first one that succeeds. 
-}
connect : List (Observer action obs) -> Observer action obs
connect observers subaction = 
  pick ((|>) subaction) observers


{-| Given a lifting function, a list of observers and an embedding, construct a 
Part. 
-}
new'
  : (Action container obs -> obs) 
  -> List (Observer action obs) 
  -> Embedding model container action a 
  -> Part model container action obs a
new' lift observers embedding = 
  let 
    fwd = 
      pack (observe (connect observers) embedding.update) >> lift
    get = 
      embedding.getModel
    set = 
      embedding.setModel
  in
    { view = 
        \addr -> 
          embedding.view (Signal.forwardTo addr fwd) 
    , get = get
    , set = set
    , map = \f model -> set (f (get model)) model
    , fwd = fwd
    }



{-| It is helpful to see parameter names: 

    new view update get set id lift model0 observers = 
      ...

Convert a regular Elm Architecture component (`view`, `update`) to a part, 
i.e., a component which knows how to access its model inside a generic
container model (`get`, `set`), and which dispatches generic `Action` updates,
lifted to the consumers action type `obs` (`lift`). You can react to actions in
custom way by providing observers (`observers`). You must also provide an
initial model (`model0`) and an identifier for the part (`id`). The
identifier must be unique for all parts of the same type stored in the
same model (overapproximating rule of thumb: if they are in the same file,
they need distinct ids.)

Its instructive to compare the types of the input and output views:

    {- Input -}                 {- Output -}
    View model action a         View container obs a

That is, this function fully converts a view from its own `model` and `action`
to the master `container` model and `observation` action. 
-}
new
  : View model action a
  -> Update model action
  -> (container -> Indexed model)
  -> (Indexed model -> container -> container)
  -> Int
  -> (Action container obs -> obs)
  -> model
  -> List (Observer action obs)
  -> Part model container action obs a

new view update get set id lift model0 observers = 
  embedIndexed view update get set model0 id 
    |> new' lift observers


{-| Variant of `new` for parts that will be used only once in any 
TEA component. 
-}
new1
 : View model action a
  -> Update model action
  -> (container -> Maybe model)
  -> (Maybe model -> container -> container)
  -> (Action container obs -> obs)
  -> model
  -> List (Observer action obs)
  -> Part model container action obs a

new1 view update get set lift model0 observers = 
  embed view update (get >> Maybe.withDefault model0) (Just >> set)
    |> new' lift observers


-- HELPERS



map2 : (b -> b') -> (a, b, c) -> (a, b', c)
map2 f (x,y,z) = (x, f y, z)


map1st : (a -> c) -> (a,b) -> (c,b)
map1st f (x,y) = (f x, y)



