---
layout: post
title: Extending Syntax in Scala
date: 2015-11-05 14:29 -0500
category: tutorials
tags:
  - syntax
  - operations
---

Yesterday I learned something really cool. In Idris I can just do this
to extend the syntax:

```idris
syntax [ma] "?" [a] = fromMaybe a ma
```

where `fromMaybe` is from the `Prelude.Maybe` module which has the type
signature: `fromMaybe : Lazy a -> Maybe a -> a` so now I can do:

```idris
idris> (Just 88) ? 77
88 : Integer

idris> Nothing ? 77
77 : Integer
```

So today we will see how much of a challenge it will be to do this in Scala.

_Hint: It might be a little more work, but doable._

## Setup

We will start out defining the sum type `Maybe` with data constructors `Nowt`
(same as `Nothing` in Idris) and `Just` which wraps an underlying `A` value.

```tut
sealed abstract class Maybe[A]
case class Just[A](a: A) extends Maybe[A]
case class Nowt[A]() extends Maybe[A]
```

Now we just have to define the `fromMaybe[A]` decorator. We can do this in the
companion object:

```tut
object Maybe {
  def fromMaybe[A](a: A)(ma: Maybe[A]): A = ma match {
    case Just(x) => x
    case Nowt() => a
  }
}
```

Now we have what was already provided by Idris in the Prelude, therefore so
far, we have been setting up our Scala code to mimick what we had in Idris.
(Note: we could have also used `Option[A]` and related `getOrElse` method
instead of the above).

```tut
object Maybe { self => /* Note: newly added reference to outer object */
  def fromMaybe[A](a: A)(ma: Maybe[A]): A = ma match {
    case Just(x) => x
    case Nowt() => a
  }

  /* Helpers for value construction */
  def apply[A](a: A): Maybe[A] = if (a == null) Nowt() else Just(a)
  def nowt[A]: Maybe[A] = Nowt()
  def just[A](a: A) = Maybe(a)

  /* Newly added code */
  implicit class MaybeOps[A](val ma: Maybe[A]) extends AnyVal {
    def ?(a: A): A = self.fromMaybe(a)(ma)
  }
}
```

Now we can test that we can use the new syntax (`?` operator) the way we want:

```tut
object Main extends App {
  import Maybe._

  val mint1 = just(30)
  val mint2 = nowt[Int]

  println(mint1 ? 60)
  println(mint2 ? 60)
}

Main.main(Array.empty[String])
```

We added decoration method `?` to `Maybe[A]` values by importing the companion
object implicit value class `MaybeOps[A]` definition into scope.

To recap we only need to add the following code to _provide_ the syntax
within the companion object:

```scala
  implicit class MaybeOps[A](val ma: Maybe[A]) extends AnyVal {
    def ?(a: A): A = self.fromMaybe(a)(ma)
  }
```

On the consuming side we just need to import the top-level companion object
content into scope like so:

```scala
  import Maybe._
```

For bonus points, we added a couple of smart value constructors in the
companion object:

```tut
  def apply[A](a: A): Maybe[A] = if (a == null) Nowt() else Just(a)
  def nowt[A]: Maybe[A] = Nowt()
  def just[A](a: A) = Maybe(a)
```

This makes consuming code for constructing `Maybe[A]` values simpler
with appropriate type inference (i.e. we want `Maybe[A]` to always
be inferred not `Just[A]` or `Nowt[A]`).

If we had not added these value constructors or not used them we
would have this problem on our hands:

```tut
  val mint1 = Just(30)
  val mint2 = Nowt[Int]
```

Look at the types inferred by Scala. In some scenarios, this might
give you problems.

## Review

Scala provides a way to extend syntax in a few different ways. Above we
showed one way where we can still uphold the level of typesafety that our API
requires yet still offer some level of syntax/operator decoration or addition.

How could you effectively enhance your operations management libraries with
the technique outlined above?
