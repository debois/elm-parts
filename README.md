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

    elm-make examples/1.elm

Its instructive to see the diff between 
[1.elm](https://github.com/debois/elm-parts/blob/master/examples/1.elm) (1 counter)
and
[2.elm](https://github.com/debois/elm-parts/blob/master/examples/2.elm) (2 counters). It's just this: 
```patch
    38a39,43
    > counter1 : Counter.Part Model Action
    > counter1 =
    >   Counter.part 1 PartAction 0 [] 
    > 
    > 
    43a49
    >     , counter1.view addr model
```

That is, all you need to do is create an additional part (first change), then use
it in your view-function (second part). 
