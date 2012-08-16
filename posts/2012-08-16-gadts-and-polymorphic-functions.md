---
title: GADTs and fully polymorphic functions
date: Aug 16, 2012
---

Today I've been asked a question about GADTs and the answer turned out
to be somewhat tricky, so I decided to clarify it for myself in this
post.

### "Default values" example

Let's start with some contrived example and say we want to have default
values for certain types. We can use typeclasses for this, but let's
proceed with [GADTs](http://en.wikibooks.org/wiki/Haskell/GADT):

~~~{.haskell}
{-# LANGUAGE GADTs #-}

module Main where

data Type a where
  TBool :: Type Bool
  TInt  :: Type Int

~~~

Here we have a bare representation of certain Haskell types. Now, our
function for getting defaults may be written like this:


~~~{.haskell}
def :: Type a -> a
def t = case t of
  TBool -> False
  TInt  -> 0

~~~

So far, so good. But wait, the patterns `TBool` and `TInt` have `Type
a` type, but `a` parameter is different in each pattern! How can they
coexist? And will this work without GADTs?

### Polymorphic functions

Let's try. Assume this, somewhat artificial, code:

~~~{.haskell}
f :: [a] -> a
f x = case x of
   [True] -> True
   [1]    -> 1

~~~

Here we want to achieve something similar: a polymorphic function which
have different `a` in its clauses. But this clearly won't work, even if
left only with the first clause:


~~~{.haskell}
g :: [a] -> a
g [True] = True

~~~

GHC error is the following:

~~~
  "Couldn't match type `a' with `Bool':
    `a' is a rigid type variable bound by the type signature
      for g :: [a] -> a"
~~~

So what's happening here? In the type signature we say `g :: [a] -> a`
which means that we promise that `g` should return a truly polymorphic
value, not a dull `Bool`, as we do in this case. The same is true about
the patterns: we are not allowed to use concrete types in the
patterns, since this will break polymorphism and our promise that `g`
should work for the list of any type `a`.

Even simpler example of the problem would be the following function,
which gives a similar typing error:

~~~{.haskell}
g :: a
g = "aaa"

~~~

This fails. One might think - hey, a `String` may be an `a` too, why
not to cast it automatically, like we do, say, in Java?


~~~{.java}
Object g() {
  return "aaa";
}

~~~

The problem with that approach is that in Haskell the *caller* of the
function relies on the fact that it's polymorphic and decides what `a`
variable actually is. For example, suppose we are using `g` function
in the following code:


~~~{.haskell}
h :: Int
h = 1 + g

~~~

This is a perfectly legal piece of code, since `g` supposed to be fully
polymorphic, thus it might be used as an addend too. But if `g` would
have returned a string "aaa", this certainly won't be the case.

On the other hand, in Java, the *callee* (not the caller) decides
which type is actually used for `a`, and the only thing that caller
knows is that certain interface is supported. So the callee may decide
that it's perfectly fine to return a String "aaa", as soon as it
conforms to the Object interface. And the caller might only use Object
methods to do something with that String. So, we can't implement `h`
analog in Java, because `Object` does not expose any methods for
addition.

This leads to further question: are there any reasonable implementations
of function with the type signature `g :: a`? To be a valid
implementation, the returned value has to be of every possible type.
There is only one such value: &perp;, so the only possible implementation
is:


~~~{.haskell}
g = undefined

~~~

### Back to GADTs

Ok, now we have seen that such functions won't compile and understood
why, but why it works with GADT?

~~~{.haskell}
def :: Type a -> a
def TBool = False
def TInt  = 0

~~~

Let's take a closer look on the first clause:


~~~{.haskell}
def TBool = False

~~~

The right hand side of the definition is obviously of type `Bool`, not
`a`. However, if the argument to `def` is `TBool`, then the type
parameter `a` must be exactly `Bool`, this restriction is clearly
specified in GADT definition:


~~~{.haskell}
-- a is Bool here!
TBool :: Type Bool

~~~

So the right hand side has type `a` in this context. Similarly, the
RHS of the second definition has type `Int`, but only in the context
when `a` must be `Int`. Therefore, everything fits in place and we
have a well-typed program.

### Moral

This pattern matching with simultaneous type refinement seems to be one
of the key features of GADTs. It allows to refine general type variables
like `a` to something more concrete with the help of user-defined type
refinement (specified in GADTs definition), which in turn allows writing
so much nicer code!
