---
layout: post
title: Algebraic Data Types
---

*NOTE: This is an edit of an article I wrote for [Functional
Algebra](http://functionalalgebra.com) in 2012.*

## Prerequisites

* Scala 2.10.x or higher

## What forms of Algebraic Data Types (AlgDTs or ADTs for short) are there?

An _algebraic data type_ can take many forms:

 * Sum type
 * Product type
 * Recursive type
 * Generic type
 * Hybrid combination of any of above

We will walk through examples of sum, product, recursive, and
generic types below.

## Sum Types (aka Tagged Unions)

Synonyms:

* tagged union
* coproduct
* sum type

Similar:

* union type
* enum(eration)

Related:

* product type (or really it's the categorical dual of a sum type)

Applications:

* a type that has a (humanly) small set of mutually exclusive
  value constructions, e.g.
  * `data Boolean = True | False`
  * `data UserType = Anonymous | Subscriber | Editor | Admin`
  * `data CardSuit = Hearts | Spades | Diamonds | Clubs`

Definition:

> The notion of a type having a known, and exhaustive list of
> constructors where the type must be constructed by exactly
> one of these is known as a *Tagged Union*.
> I have also seen it called a *sum type*. In fact,
> I much prefer the latter so expect me to use that form in
> this and later articles.

A *sum type* is a *concrete* type definition where
our type is one of `N` known *constructions*, where `N` is a
finite positive integer. Each construction may hold zero or
more components of specific types (although these components
may vary from construction to construction.

Practically speaking `N` should be reasonably small most
of the time (in the single or double digits) simply because
all good software developers are lazy and dislike boilerplate
code! :)

Let's step back to a basic example of a _sum type_ (which
is one variety of AlgDTs) to illustrate when you should use
this notion and what benefits it offers over free form type
definitions which are open.

Let's fire up a `scala` REPL process and paste in the following:

```tut:book
object coin0 {
  sealed trait USCoin                 { def value: Int  }
  case object Penny extends USCoin    { def value = 1   }
  case object Nickel extends USCoin   { def value = 5   }
  case object Dime extends USCoin     { def value = 10  }
  case object Quarter extends USCoin  { def value = 25  }
  case object Dollar extends USCoin   { def value = 100 }
}
```

The above just tells Scala that there is this trait (in this case
we can pretend it is the same as the LHS of Haskell's data
declaration, but it isn't always this way), <code>USCoin</code>,
that has a finite, and known number of constructors up front
(typically how case classes in Scala get used [well]). Note
that the trait is <code>sealed</code>. For non-Scala people this
just means that outside of this source file, no extensions of
this trait are allowed. i.e. the compiler can guarantee that
consumers of our libraries, or toolkits cannot extend USCoin.
In this particular scenario that is probably what we want (not
allowing consumers of our code to extend this). The likelihood
that the US central bank would introduce new coins or take
existing coins out of circulation before we update our library
in time to cater for it, is pretty unlikely. However, there is
another good reason why we might want this too: we can know we
have exhaustively catered for all constructions of USCoin in our
supporting logic or calculations.

Note: we could have also defined it this way:

```tut:book
object coin1 {
  sealed abstract class USCoin(value: Int)
  case object Penny    extends USCoin(1)
  case object Nickel   extends USCoin(5)
  case object Dime     extends USCoin(10)
  case object Quarter  extends USCoin(25)
  case object Dollar   extends USCoin(100)
}
```

You might be wondering how this can be done. Let us try to use
this code to explore this idea further. So back in your
running `scala` REPL:

```tut:book
object VendingMachine {
  // import your preferred implementation below
  import coin0._

  // Define predicate for a vending machine that only accepts
  // dimes and quarters (common today).
  def accepts(coin: USCoin): Boolean = coin match {
    case Penny    => false
    case Nickel   => false
    case Dime     => true
    case Quarter  => true
  }
}
```

What happened here is that we forgot to match on the
`Dollar` constructor for `USCoin` _sum type_ and the Scala
compiler *warned* us about it (by default but you can make the
Scala compiler treat this warning as a compile error if you like).

If you find you want this behaviour for a particular data type
definition, then you probably want to define it as a sum type
this way.

## Product Types (aka Record Types)

Synonyms:

* product type
* record type

Similar:

* Tuple (this is the simplest kind of product)

Related:

* sum type (is a dual of product type)

Applications:

* anything that can be represented as a tuple of values.
  e.g. `User(123, "mbbx6spp", "mbbx6spp@...",
            "Snarky functional programmer")`
Definition:

> In the language of abstract algebra, which computer scientists
> in programming language theory have adopted, a data
> type with exactly one constructor is _isomorphic_ to
> what is often called a _product type_ that takes an ordered
> list of typed <em>operands</em>. Informally these are
> sometimes called *Record Types*.

[@chvest](https://twitter.com/chvest) pointed out while
reviewing this article that the archetypal product type is
the tuple. For example, perhaps we want to represent an image
element in a HTML page. We might initially represent an image
element as the following tuple (via a type alias in Scala below):

```tut:book
type Image = (String, Int, Int)
```

Here we take a string (the source URL), an integer (the width),
and a second integer (the height). The problem with tuples is
that this might also represent any number of issues. It is hard
to know what it is referring to. *Enter product types.*

In Scala below we can represent an image element in HTML as the
following case class.

```tut:book
case class ImageElement(sourceUrl: String, width: Int, height: Int)
```

Since pure product types only have one constructor for a type,
we can eliminate the trait or abstract class `type` definition
that we used in the sum type example above with `USCoin`.

### Sum-Product Type Hybrids

<img src="{{ site.url }}/images/fbtopbar.png" alt="Facebook topbar">

How about a social network event notification. Let's take Facebook.
Whenever you sign into Facebook (or whatever social network you
might use), you will likely be greeted with a visual indication
of any notifications you might have since you last signed in.
Things like friends _liking_ your statuses, comments, or
making comments on your posts or posts you have commented on
yourself.

<img src="{{ site.url }}/images/fbnotifications.png" alt="Facebook notifications">

So a first stab at modeling this with an algebraic data type in
Haskell might look like:

```haskell
import Data.DateTime

-- Assumes User is defined elsewhere and imported

-- Assume Status and Comment types are already defined
data Notification = CommentNotification Status [User] DateTime
                  | CommentLikeNotification Comment [User] DateTime
                  | PostLikeNotification Status [User] DateTime
```

What did I actually do? Well if we look at the different
notifications we see there are a variety of constructions of
notification events, including:

* When one of your statuses is liked it tells you who and at what
  time last action was taken
* When one of your comments is liked it tells you who and the time
  last action was taken
* When a post you commented on is commented on it tells you who
  and the time last action was taken
* ...


Let us dissect the Haskell code a little. The identifier on the
LHS, `Notification`, is the type. Then the RHS contains an
exhaustive list of possible _*constructors*_ such as:
`CommentNotification`, `CommentLikeNotification`,
`PostLikeNotification` for our simple model.

Now we could have modeled `Notification` data type a little
differently. Let us consider the following modeling of the domain:

```haskell
import Data.DateTime

-- Assumes User is defined elsewhere and imported

-- We might want to add more constructors for Post sum type of a more
-- complete model of Facebook notifications, but left as a homework to
-- reader, because every algebra lesson has this ;)
data Post = Status Text DateTime | Comment Text DateTime
data Notification = Notification Post [User] DateTime
```

This model of the domain of Facebook notifications uses a pure
_product type_ for the `Notification` type definition and a
hybrid sum-product type for the `Post` type.

Exactly how you model this domain will depend on what properties
you would like the various types to possess. In fact, there are
many ways you can model this domain with various forms of
algebraic data types (AlgDT). It all depends on your program's
view of the domain.

There is no one presice view of a particular domain, it depends
on your program's functionality. For example, if you are
building a front office trade posting system, you are unlikely to
need to model the historical view of a security or instrument
type. You just need to know the active identifier (e.g. ticker,
ISIN, CUSIP, internal id, etc.) for the security being traded
today to flow through correctly.

However, a knowledge management tool may need to model a security
or instrument to have a historical view (e.g. how a corporate
action impacted a security in the past, or the historical view
of identifications or vendor symbols).

## Recursive Types

Synonyms:

* recursive data type
* recursively-defined data type
* inductive data type
* inductively-defined data type

Related:

* dynamically defined data structures

Applications:

* structures that build out dynamically defined directed graphs,
  e.g. lists, trees, etc.

Definition:

> A recursive type is a type that is defined in terms of itself.

We will explore this form of algebraic data type using the example
of a binary tree with integer values:

```tut:book
sealed trait IntTree
case class IntLeaf(value: Int) extends IntTree
case class IntBranch(left: IntTree, right: IntTree) extends IntTree
```

We can now build up trees with integers at the leaves like so:

```tut:book
object inttrees {
  val t0: IntTree = IntLeaf(9)
  val t1: IntTree = IntBranch(IntLeaf(6), IntBranch(IntLeaf(4), IntLeaf(8)))
}
```

In Haskell we can express the above example with a little less
syntactic clutter like so:

```haskell
data IntTree = IntLeaf Int | IntBranch IntTree IntTree

t0 = IntLeaf 9

t1 = IntBranch (IntLeaf 6) (IntBranch (IntLeaf 4) (IntLeaf 8))
```

You will note in the above definition given the possible
constructions that no empty `IntTree` can be constructed.

For bonus points: what would you do to the definition above to
allow for an empty `IntTree` value construction?

## Generic Types

Synomyms:

* parametric types
* generic types

Similar:

* higher-kinded polymorphism

Related:

* Should *not* be confused with Generalized Algebraic Data Types
  which are a whole other topic, though related to algebraic
  data types.
* concrete types are instantiations of a generic type after
  the type argument to the type constructor. Note: disctinction
  between type constructor and value or data constructor.

Applications:

* any type where a constituent element of the algebraic data type
  can vary across uses.
* can be used to create phantom types when needed.

Definition:

> A type paramaterized on another type at type construction.

For this form of type that is supported in many mainstream
functional programming languages like Haskell, Scala, and
OCaml we will explore how to define a type that parameterizes
on a record constituent of the definition:

```tut:book
sealed abstract class GenericTree[A]
case class Leaf[A](value: A) extends GenericTree[A]
case class Branch[A](left: GenericTree[A], right: GenericTree[A]) extends GenericTree[A]
```

This allows us to use the same structure with different specific
types such that we can build a tree with a `String` at each leaf
with the same generic code as that used for tree values constructed
with `Int` values at the leaves.

## Conclusion

This was a whirlwind introduction to algebraic data types and
related ideas. There are a lot more relationships and assocations
that can be explored w.r.t. algebraic data types but I hope this
provides a simple, understandable introduction with compiling
examples.

Cheers.

PS If you find any errors, heckle me on twitter
[@SusanPotter](https://twitter.com/SusanPotter).
