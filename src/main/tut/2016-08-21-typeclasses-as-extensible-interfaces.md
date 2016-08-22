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

* `Comparable<T>` in Java, `Ord a` in Haskell, etc. This interface
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
as user of the interface you cannot be sure if the subclass
as overridden the appropriate methods to provide a meaningful
- to your app - implementation. Creating an explicit interface
adds overhead but allows the user of the interface to know
the implementation provides an implementation for the specific
need at hand.

In Scala we might write the following:

```tut:book
trait Showable[A] {
  def show(a: A): String
}
```

If I wrote a type that wanted to implement this interface what
could I do? Since we are not going down the obvious subtyping
route (or this post would be boring to most) I propose we utilize
Scala's use of implicits:

```tut:book
sealed abstract class Environment(val name: String) {
  case class Dev(tag: String) extends Environment(s"${tag}@dev")
  case class Staging(tag: String) extends Environment(s"${tag}@staging")
  case class Prod(tag: String) extends Environment(s"${tag}@prod")

  implicit val showEnv: Showable[Environment] =
    new Showable[Environment] {
      override def show(e: Environment): String =
        s"env://${e.name}"
    }
}
```


