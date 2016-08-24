---
layout: post
title: Typeclasses as truly extensible interfaces
---

## The problem with Algebraic Data Types

In the previous post on [Algebraic Data Types]()
we saw a variety of commonly supported forms of ADTs as
well as how and why we would use them along with examples.

What you may have noticed is that all those forms are
_closed_. By this I mean at the time of definition we
must know all ways we can construct values of that type.
This is not extensible outside of the data type declaration
or source file. Stated another way, we cannot take an algebraic
data type defined in a library we depend on and _extend_ the
ways in which we can construct values of it after it is declared.

You might be thinking ADTs aren't going to solve this problem
alone. You would be right, so let's look at how we can define
interfaces that are open to new implementations outside of
the context of its definition.

This post will review what interfaces are with examples, walk
through an example of building an explicit interface for a
common need in a couple of languages, and apply this to
an infrastructure example.

## Interfaces 101

Let's step back to review the purpose of interfaces in software:
to define a set of related behaviors on a type or set of types
that conform to a predefined _contract_.

Some examples of well established interfaces in software are:

* `Comparable<T>` in Java, `Orderable a` in Haskell, etc. This interface
  defines the way in which values of a type can be ordered or
  sorted.
* `InputStream` and `OutputStream` for byte-based I/O interfaces
  in Java.
* `Reader` and `Writer` for character-based I/O interfaces
  in Java.
* `Enumeration` or `Iterator` in Java and similar in C#, etc.
  See also `Enumerable` in Ruby.

As programmers with experience in mainstream languages such as
Java, C#, Ruby, Python, etc. we are familiar with many SDK or
standard library distributed interfaces like those above.

## `Showable` typeclass

In this section we look at creating an explicit interface
for `toString()` (Java), `Object#to_s` (Ruby),
`__str__/__repr__` (Python), and similarly for equivalents other
languages.

Why wouldn't we just use the builtin capabilities of the
language? In the examples provided they are all defined
with a meaningless general implementation such that as
a user of the interface you cannot be sure if the subclass
has overridden the appropriate methods to provide a meaningful
- to your app - implementation. Creating an explicit interface
adds _some_ overhead but allows the user of the interface to know
the implementation provides an implementation for the specific
need at hand, not a generic implementation that just spits out
the hashCode or memory address of the object.

In Scala we might write the following to depict an interface
for the specific need:

```tut:book
trait Showable[A] {
  def show(a: A): String
}
```

If I declared a type that needed to implement this interface what
could I do? Since we are not going down the obvious OO subclassing
extending/implementing route (or this post would be boring to
most) I propose we utilize Scala's use of implicits:

```tut:book
sealed abstract class Environment(val name: String) {
  def show(implicit ev: Showable[Environment]) = ev.show(this)
}

object Environment {
  case class Dev(tag: String) extends Environment(s"${tag}@dev")
  case class Staging(tag: String) extends Environment(s"${tag}@staging")
  case class Prod(tag: String) extends Environment(s"${tag}@prod")

  /* smart constructors */
  def dev(tag: String): Environment     = Dev(tag)
  def staging(tag: String): Environment = Staging(tag)
  def prod(tag: String): Environment    = Prod(tag)

  implicit object ShowEnv extends Showable[Environment] {
    override def show(e: Environment): String =
      s"env://${e.name}"
  }
}
```

We might see how we use the API in the code example below:

```tut:book
import Environment._

dev("mbbx6spp").show
```

What we did here is define an instance method on `Environment`
that will be inherited in each of the case class concrete
subtypes. We ask the caller for _evidence_ that the `Environment`
type has an implementation for `Showable[_]` that can be in any
of the implicit scope lookup. Inside the companion object of
the `Environment` type we define the implicit definition
for that implementation. We then delegate to the `show`
method of the `Showable[Environment]` implementation.

What this implicit evidence mechanism in Scala allows us to
do is define the domain in terms of algebraic data types to
describe the structure of the data and then implement behaviors
to be defined independent of the type definitions. This means
we are no longer forced to implement all the possible
interfaces at the base class to avoid brittle object hierarchies.
We just add the definition for the desired behavior in the
application or library where we need it.

## Ord Environments

Now we will take a look at another use case that will take
advantage of the openness of defining interface implementations
with typeclasses rather than at the top of a class hierarchy.

Those that are familiar with mixins in dynamically typed languages
like Python and Ruby may notice conceptual similarities with
typeclasses as defined so far here.

One notable difference to Scala typeclasses is that the
implementations of the interface (aka typeclass instances)
are typechecked at compile-time. This provides type safety
that does not exist in Ruby or Python mixins. If not all
the methods of an interface are provided in a mixin module
this will only error at run-time in Ruby or Python using mixins.
Ideally I would like to be told about this at compile time when
all the information can be known.

Now let's look at how we can take advantage of this technique
to define ordering of `Environment` s using a generic interface
where we can utilize a rich set of operators or functions without
marrying the type definition with the `Orderable` interface itself.

The idea here is that we want to specify a strict order of
`Environment` s for promotion pipeline purposes (I am
trivializing) for the sake of an example to build out.

```tut:book
// Don't use this in your real code, this is only for demonstration
// purposes as scalaz has all this stuff with a couple of
// differences in implementation choices.

// This file demonstrates a port of the Orderable type class
// from Haskell to Scala to show how you might do this for
// your own typeclasses, since there are many techniques of
// doing this in Scala, this just shows one way, which you
// might try to start off with until you get comfortable
// with the way the suits you best.

trait OrdTypes {
  // First we start off with our "initial algebra" of algebraic data type
  // definitions of our problem space.
  sealed trait Ord
  final case object LessThan 		extends Ord
  final case object EqualTo 		extends Ord
  final case object GreaterThan extends Ord

  // Now this is the trait that defines our "typeclass" in Scala.
  // This is a slight modification from what we see in Haskell
	// since we only need to implement one trait method for
	// this type class and the rest of the definitions of the
	// functions from the Haskell typeclass come for free, I
	// decided to separate them.
  trait Orderable[A] {
    def compare(a1: A, a2: A): Ord
  }

  // Here are all the functions we may want to import into the
	// local scope where we would want to use them.
  object Orderable {
    def minimum[A](a1: A, a2: A)(implicit ord: Orderable[A]): A =
      if (ord.compare(a1, a2) == LessThan) { a1 }
      else { a2 }

    def maximum[A](a1: A, a2: A)(implicit ord: Orderable[A]): A =
      if (ord.compare(a1, a2) == GreaterThan) { a1 }
      else { a2 }

    def minimum[A](xs: Seq[A])(implicit ord: Orderable[A]): Option[A] = xs match {
      case x :: rest =>
        Some(rest.foldLeft(x) { (a: A, b: A) =>
          if (minimum(a, b)(ord) == a) { a } else { b } })
      case Nil => None
    }

    def maximum[A](xs: Seq[A])(implicit ord: Orderable[A]): Option[A] = xs match {
      case x :: rest =>
        Some(rest.foldLeft(x) { (a: A, b: A) =>
          if (maximum(a, b)(ord) == a) { a } else { b } })
      case Nil => None
    }

    def isMinimum[A](a1: A, a2: A)(implicit ord: Orderable[A]): Boolean =
      (ord.compare(a1, a2) == LessThan)

    def isMaximum[A](a1: A, a2: A)(implicit ord: Orderable[A]): Boolean =
      (ord.compare(a1, a2) == GreaterThan)

    def ===[A](a1: A, a2: A)(implicit ord: Orderable[A]): Boolean =
      ord.compare(a1, a2) == EqualTo

    def =/=[A](a1: A, a2: A)(implicit ord: Orderable[A]): Boolean =
      ord.compare(a1, a2) != EqualTo
  }
}

trait OrdInstances extends OrdTypes {
  implicit val IntOrderable = new Orderable[Int] {
    def compare(x: Int, y: Int): Ord =
      if (x < y)      { LessThan }
      else if (x > y) { GreaterThan }
      else            { EqualTo }
  }

  implicit val DoubleOrderable = new Orderable[Double] {
    def compare(x: Double, y: Double): Ord =
      if (x < y)      { LessThan }
      else if (x > y) { GreaterThan }
      else            { EqualTo }
  }
}

trait OrdUsage extends OrdTypes with OrdInstances {
  val unorderedInts: List[Int] =
    5 :: 4 :: 7 :: 9 :: Nil

  val unorderedDoubles: List[Double] =
    6.3 :: 7.0 :: 3.2 :: 3.14159 :: 1.2 :: 95.4 :: Nil

  // Try:
  //scala> Orderable.maximum(unorderedInts)
  //res1: Option[Int] = Some(9)

  //scala> Orderable.minimum(unorderedInts)
  //res2: Option[Int] = Some(4)

  //scala> Orderable.minimum(unorderedDoubles)
  //res3: Option[Double] = Some(1.2)

  //scala> Orderable.maximum(unorderedDoubles)
  //res4: Option[Double] = Some(95.4)
}
```

The above code provides a generic interface and some common
implementations of the `Orderable` interface for numeric types
such as `Int` and `Double`.

This only needs to be defined once and if you are using (like
you should be) a library with common typeclasses and standard
type implementations of them (e.g. Scalaz, or Cats) then
just import them into your application code. The code
directly above is only for demonstrating these ideas.

Now the code below is how we would specify the _instance_
impelementation for the `Orderable[Environment]` use case:

```tut:book
```
