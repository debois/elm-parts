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
