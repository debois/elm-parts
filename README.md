# elm-parts

The Elm Architecture (TEA) is conceptually very nice, but it forces us to write
large amounts of boilerplate whenever we need to use a component.  We must:

  1. Retain the state of the component in our Model 
  2. Add the components actions to our Action 
  3. Dispatch those actions in our update

None of these things have anything to do with what we want from the component, 
namely rendering it in our View function, and potentially reacting to some 
(but not all) of its actions---e.g., we want to react to a Click of a button, 
but we don't care when it updates its animation state. 

This module provides an extensible mechanism for lifting arbitrary
(differently-typed) TEA components into components having the same type of Model
and Action, all of which can be dispatched by the single `update` function in
this module. We call such a lifted component a "part" (think "interchangeable
parts".)

# Drawbacks

1. Elm-parts moves work from component consumers to component authors. As a 
component author, you have to produce some elm-parts boilerplate. Producing 
this boilerplate is prone to sometimes difficult-to-debug type errors. 

2. Elm-parts relies on sending functions in messages, in violation of the
[recent guidelines](https://github.com/evancz/elm-sortable-table#usage-rules). 
This means consumers of components trying to debug parts messages with 
the idiomatic
  ```elm
    update msg model = 
      case Debug.log "update" msg of 
        ...
  ```
will see an unhelpful `Msg <function>` rather than the actual `Msg` being sent. 

3. If component users need to access the `Model` of your component or send
messages to its `update` function, you'll need to write additional boilerplate
for the component, exacerbating (1) above. 

One appropriate use-case for elm-parts, where you can accept these drawbacks, 
is when writing a UI-component library. (Incidentally, its the only such 
use-case I know of.) As a library author, (1) you should be
willing to take upon yourself the tediousness of (1) for your users. (2) Your 
users shouldn't care about your internal messages anyway.
And (3) UI-components generally, but not always, can be written such that they 
are configured exclusively through view (3)---again, see [sortable table](https://github.com/evancz/elm-sortable-table/blob/master/examples/1-presidents.elm).

Elm-parts was developed and is in active use as a supporting library for 
[elm-mdl](https://github.com/debois/elm-mdl).

# Examples

The 
[examples folder](https://github.com/debois/elm-parts/tree/master/examples)
contains the first two steps of the 
[Elm Architecture Tutorial](https://github.com/evancz/elm-architecture-tutorial)
where the Counter is implemented as a part. To build, e.g., step 1:

    elm-make examples/0-counter-part.elm

Its instructive to see the diff between 
[0-counter-part.elm](https://github.com/debois/elm-parts/blob/master/examples/0-counter-part.elm) (1 counter)
and
[1-counter-pair-part.elm](https://github.com/debois/elm-parts/blob/master/examples/1-counter-pair-part.elm)
(2 counters). It's just this: 
```patch
60a61
>     , Counter.render CounterMsg [1] model
```
That is, additional instances of a single component require changes __only__ where you 
render them, in your `view` function. 
