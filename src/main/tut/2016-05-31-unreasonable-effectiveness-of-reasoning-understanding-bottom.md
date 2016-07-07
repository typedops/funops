---
layout: post
title: 'The Unreasonable Effectiveness of Reasoning: Eliminating Bottom'
date: 2016-05-25 19:50 -0500
category: tutorials
tags:
  - types
  - referential transparency
  - bottom
---

This article provides a simple code-level case study to understand
how two functional programming libraries have made different trade-offs. We
explore related API-level design choices in a neutral language (Scala) where
we consider the ability of the consumer of the API to reason equationally
about it. We demonstrate that focusing on eliminating bottom yields
more _reasonable_ (equationally) APIs.

## Background: (Mathematical) Bottom (⊥)

Wikipedia states this as the definition:

> In type theory, a theory within mathematical logic, the bottom type is the
> type that has no values. It is also called the zero or empty type, and is
> sometimes denoted with falsum (⊥).
>
> A function whose return type is bottom cannot return any value. In the
> Curry–Howard correspondence, the bottom type corresponds to falsity.

In lay person's terms (my own words, not formal at all):

> bottom (in mathematics/type theory) represents any _false_ situation where
> you can no longer use equational reasoning. E.g. any time you raise an
> exception that unwinds the stack, or a system error is raised from the
> bowels of the runtime, you have unterminated functions, or you otherwise
> inflict the operating environment your program runs within with an unspoken
> side effect.

Real world bottom:

* returning `null` (Java, Scala), `nil` (Ruby), `None` (Python), etc.
* throwing/raising an exception
* non-terminating functions (a function that will never normally return
  unless it encounters an exception); this is needed for building long
  running servers or operating systems.

### References

 * https://en.wikipedia.org/wiki/Bottom_type
 * https://wiki.haskell.org/Bottom

### Implications

(Pure) Functional programming is predicated on proven logic results from the
lambda calculus. Any time your code or other dependencies return a
_"bottom"_ value from a function you have violated the contract that allows
you to reason mathematically about your code. In truly exceptional cases
this is necessary to have an operationally well-behaved software in the real
world. Using exceptions for control flow is not _truly exceptional_.

One _truly exceptional_ example might be a runtime exception to denote an out
of memory system condition was met. In such a case, what can your application
reasonably do to counter this problem? I would argue nothing in most
runtimes, except maybe log it and exit if that is even possible.

Knowing how, when and what to reason about in your codebase is the key to
applying the lambda calculus to your software in the real world. Even
Haskellers have portions of their code they cannot equationally reason about
to cater for their real world operational needs. Knowing the vulnerabilities
in the ability to reason about your code is key building more well reasoned
software.

Limiting the surface area where _bottom_ may arise in your code reduces the
possibility of wide classes of runtime bugs albeit at _possible_ (not always)
costs of more resource usage.

## Case Study: System Environment Variable Lookup

Doing something as simple as looking up an environment variable yields
_bottom_ in OCaml but not in Haskell/GHC. Let's compare approaches.

### OCaml 4.01.0

The version of OCaml can likely vary from this version to yield the
same results, but I have only tested with 4.01.0.

```bash
[git:master?+] spotter@redhorn ~/src/work/root
$ ocaml
        OCaml version 4.01.0

# open Sys;;
# getenv;;
- : string -> string = <fun>
```

The OCaml type is simple (`string -> string`): given a `string` the function
returns a `string`. Of course, it is side effecting as we will see below.
The simplicity comes at a cost because we lost the ability to:

* describe the program's logic separately from when it is
  performed/evaluated. Once you call this function a side effect is imposed
  on the operating environment. In this case we are just querying the system
  environment which may not have negative impact, but in many other cases it
  may, e.g. setting variable value in system environment.
* denote by a specific return value that the environment variable is not set.
  The way that OCaml caters for this case is by raising an exception that
  unwinds the stack. i.e. producing a bottom value. This is not represented
  to the caller in the type signature.

### Haskell/GHC 7.10.x+

This code will likely work in many prior versions of GHC but I have only
tested on 7.10.3.

```bash
$ ghci
GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
Prelude> import System.Posix
Prelude System.Posix> :t getEnv
getEnv :: String -> IO (Maybe String)
Prelude System.Posix> :q
Leaving GHCi.
```

The type signature of getting the value of an environment variable in
`System.Posix` module is:

```hs
String -> IO (Maybe String)
```

In English this translates to: _given a String we produce a program in the
`IO` context where when performed will return a `Maybe String` value._

Notice here that the module `System.Posix` provides a function that does two
things:

* wraps the result into an IO context so that the intended program can be
  represented as an `IO` value to separate it from the performance/execution
  of the program (a core tenet of referential transparency or 'purity').
* ensures that the inner value describes the context that no environment
  variable setting is a valid value in the domain of environment variables.
  i.e. what we are saying is that in a system environment if a key is not
  present, we return a value that signifies that back to the caller as an
  explicit value. In the world of environment variable retrieving we probably
  don't care about returning an error value that describes why a `String`
  value wasn't returned as there are so few reasons it would likely not be
  interesting to the caller of the function. However, in some domains we
  would provide a function with the type: `String -> IO (Either e String)`
  where `e` is a coproduct/sum type that describes the possible errors
  possible in the context of the function.

## Usage

Usage of the respective APIs.

### OCaml

Follow along in your `ocaml` REPL:

```ocaml
# getenv "HOME"
  ;;
- : string = "/home/spotter"
# getenv "HOME2"
  ;;
Exception: Not_found.
^D
```

Ouch! We do not have the ability to encapsulate the effects and decide _when_
to perform them without the context in the return type (`IO a` in the
Haskell/GHC case). We also don't receive a type expressing the optionality
of the result which forces _bottom_ be returned.

### Haskell

```bash
Prelude System.Posix> getEnv "HOME"
Just "/home/spotter"
it :: Maybe String
Prelude System.Posix> getEnv "HOME2"
Nothing
it :: Maybe String
```

Because we are in the GHCi (REPL/interactive shell) we are essentially
performing the `IO` action at the same time we call it, but that is only due
to calling within the REPL without using `let`. Below we use the same API to
get an `IO` action (aka _program_) which we can combine with other `IO`
actions to produce a new and improved program in the `IO` context to later
perform/execute it.

Follow along in your own `ghci` REPL.

```hs
Prelude> import System.Posix
Prelude System.Posix> let a0 = getE
getEffectiveGroupID   getEffectiveUserName  getEnvDefault         getEnvironmentPrim
getEffectiveUserID    getEnv                getEnvironment
Prelude System.Posix> let a0 = getEnv "HOME"
Prelude System.Posix|
a0 :: IO (Maybe String)
Prelude System.Posix> :t a0
a0 :: IO (Maybe String)
Prelude System.Posix> let a1 = getEnv "HOME2"
Prelude System.Posix|
a1 :: IO (Maybe String)
Prelude System.Posix> :t a1
a1 :: IO (Maybe String)
Prelude System.Posix System.IO.Unsafe> let a2 = a0 >>= putStrLn . printMaybeS >>= const a1 >>=
putStrLn . printMaybeS
a2 :: IO ()
Prelude System.Posix System.IO.Unsafe> unsafePerformIO a2
/home/spotter
(unset)
()
it :: ()
Prelude System.Posix> :quit
```


## Trade-offs

Let's look at each of these API choices in Scala (a neutral language):

```tut:book
/*
 * Using Scalaz Effect v7.2.1:
 *    "org.scalaz" %% "scalaz-effect" % "7.2.1"
 */
import scalaz._, Scalaz._
import scalaz.effect._

case class EnvError(message: String)

object SysEnv0 {
  /**
    * This function is side effecting:
    * We do not separate performance of the effects from
    * building the program in the context nor does it denote
    * an unset environment variable in any meaningful way (it
    * returns a null value if unset) which is another form of
    * bottom. This yields code littered with null checks diluting
    * application codebase where business logic should be central
    * focus.
    */
  def getEnv0(key: String): String =
    sys.env.get(key).getOrElse(null)

  /**
    * This function is side effecting:
    * We do not separate performance of the effects from
    * building the program in the context nor do we represent
    * optionality of the return type in any meaningful way, this
    * definition uses empty string to denote an unset environment
    * variable which is not a unique value but is not a bottom value
    * case as in the first definition. However, we lose the ability
    * to tell the case where the environment variable is not set
    * apart from the case where the environment variable was set to
    * a value of the empty string. This may not be interesting for
    * your needs, so you get to decide.
    */
  def getEnv1(key: String): String =
    sys.env.get(key).getOrElse("")

  /**
    * This function is side effecting:
    * We still do not separate performance of the effects from
    * building the program as a value and we denote the case of
    * an environment variable name not set with a runtime exception
    * which is another form of bottom value returned.
    */
  def getEnv2(key: String): String =
    sys.env.get(key).getOrElse(throw new RuntimeException(s"Key ${key} not found"))

  /**
    * This function is side effecting:
    * We are not yet separating program as a value that can be
    * combined with other programs in the same context using
    * simple and common combinators, but in this type signature
    * we explicitly denote the optionality of the returned value
    * for the case where the environment variable is not set.
    */
  def getEnv3(key: String): Option[String] =
    sys.env.get(key)

  /**
    * This function is effectful:
    * We are building a program as a value in the IO context. We also
    * denote the optionality of the underlying program's return type
    * with Option[String]. These are all good things from the perspective
    * of equationally reasoning about your code.
    */
  def getEnv4(key: String): IO[Option[String]] =
    IO { sys.env.get(key) }

  /**
    * This function is effectful:
    * We are building a program as a value in the IO context to be
    * performed at a later time once it is combined with other IO
    * programs (or values representing programs that can be translated
    * to the IO context). We also provide more context in the specific
    * error case where the environment variable is not set with a message
    * that shows a message containing the key name. This could be made more
    * explicit if we passed the value constructor for the EnvError two
    * arguments where one is the name of the environment variable and the
    * other is the message.
    */
  def getEnv5(key: String): IO[EnvError \/ String] =
    IO { sys.env.get(key) \/> EnvError(s"Couldn't find key ${key}.") }
}
```

By wrapping your return values inside effects/contexts you do require more
from the consumer of your API to get at the "happy path" result, but you
provide much more control and/or context for the consumer of your API with
additional type safety that can help in refactoring your code base. In the
non-happy path cases the extra control and context is a positive since
you would need to wrap exception handling code around the call site for
each API use.

Depending on your programming environment and requirements you may need to
weigh the memory and CPU cycle costs of this extra wrapping. Make sure you
measure whether this impacts your system negatively before prematurely
optimizing and relatively prioritize the importance of higher-level
abstraction and control over specific performance characteristics based
on application need.

There is a dirty little secret: you don't have to stop at `IO`. In fact,
it is typically preferrable to wrap/layer into more specific contexts with
limited operations/constructors and then translate to the `IO` context at the
end before performing/executing the `IO` program. You can find out more by
reviewing literature (and possibly future posts on this blog) on free monads
or extensible effects with the effect monad (sometimes referred to as `Eff`).

The moral of the story is you must remember the ability to reason about the
APIs you design are influenced by how much you make an effort to eliminate
bottom.

As the API designer you get to decide where to draw the line. It is
prudent to consider the usability of your API as a consumer. If
your library/code does not need to be highly tuned you can always provide a
simpler API on top of better defined (from an equational reasoning
perspective) functions to fit more consumer needs. Let's look below to
see what this could look like:

```tut:book
object SysEnv1 {
  def getEnv0(key: String): String =
    try {
      getEnv2(key)
    } catch {
      case re: RuntimeException => null // this is crazy, don't do this.
    }

  def getEnv1(key: String): String =
    getEnv0(key) match {
      case null => ""
      case x    => x
    }

  def getEnv2(key: String): String =
    getEnv3(key) match {
      case Some(x) => x
      case None => throw new RuntimeException(s"Key ${key} not found.")
    }

  def getEnv3(key: String): Option[String] =
    getEnv4(key).unsafePerformIO

  def getEnv4(key: String): IO[Option[String]] =
    getEnv5(key).map(_.toOption)

  def getEnv5(key: String): IO[EnvError \/ String] =
    IO { sys.env.get(key) \/> EnvError(s"Couldn't find key ${key}.") }
}
```

Cheers and happy reasoning.

### Thank you!

I would like to thank the following non-functional programmers for reviewing
a rough draft of this post for flow and cohesiveness:

* [@randomfrequency](https://twitter.com/randomfrequency)
* [Peter Caswell](https://twitter.com/PeterCaswell)
