# Pyjure internals

## Implementation Notes

The reasons why we go the hard way of specifying minimal monad stacks
rather than "just" adding an effect typesystem to pyjure and always
compiling to the full stack are that:

1- we want to be able to use the Haskell (or Frege, or Typed Clojure, etc.) typesystem
 and if possible its existing implementation, rather than reinvent and rebuild our own;
 since and our syntax transformer happens *before* things are typed, we can't use types
 to direct our transformation (although this analysis produces some kind of type, I suppose).
 We therefore need to provide Haskell with the best information we have, which would be
 destroyed if we always compiled to the full monad transformer stack. [Although, maybe
 there is a way to compile to an extensible effect framework that preserves type information?]


In a typed setting, when two functions have different effect stacks, we can recursively
unify them to the same stack. If they both have the same use (or non-use) of t_i,
that's OK, and if one doesn't, it can use the t_i lift (to be composed with further lifts
or non-lifts). It's VERY cumbersome to do manually, but that's what we want to do.
On the other hand, this type direction probably provides use with the implementation strategy:
iterate over the effects in the stack (or if notating effects by their index in the stack,
keep them ordered and iterate in order) (bitmask and ffs would be faster, but hey).


