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

### References

 * https://en.wikipedia.org/wiki/Bottom_type
 * https://wiki.haskell.org/Bottom

### Implications

(Pure) Functional programming is predicated on proven logic results from the
lambda calculus. Any time your code or other dependencies return a
_"bottom"_ value from a function you have violated the contract that allows
you to reason mathematically about your code. In truly exceptional cases
this is necessary to have an operationally well-behaved software in the real
world. One example is a runtime error that unwinds the stack to denote an out
of memory system condition was met. In such a case, what can your application
reasonably do to counter this problem? I would argue nothing, except maybe
log it and exit if that is even possible (at least in the languages and
environments I have experience in which isn't to say I know everything).

Knowing how, when and what to reason about in your codebase is the key to
applying the lambda calculus to your software in the real world. Even
Haskellers have portions of their code they cannot equationally reason about
to cater for their real world operational requirements. Knowing the
vulnerabilities in your ability to reason about your code is key to knowing
when and where you can apply equational reasoning.

Limiting the surface area where _bottom_ may arise in your code reduces the
possibility of wide classes of runtime bugs.

The rest of this article provides a simple code-level case study to understand
how functional programming libraries have made a different trade-off with
respect to the ability to reason logically about their standard library code.

## Case Study: System Environment Variable Lookup

Just doing something as simple as looking up an environment variable yields
_bottom_ in OCaml but not in Haskell/GHC. Let's compare approaches.

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

Type signature of getting the value of an environment variable in
`System.Posix` is:

```hs
String -> IO (Maybe String)
```

In English this translates to: given a String we produce a program in the
`IO` context where when performed will return a `Maybe String` value.

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

### OCaml 4.01.0

Again the version of OCaml can likely vary from this version to yield the
same results, but I have only tested with 4.01.0.

```bash
[git:master?+] spotter@redhorn ~/src/work/root
$ ocaml
        OCaml version 4.01.0

# open Sys;;
# getenv;;
- : string -> string = <fun>
```

The OCaml type is much simpler (`string -> string`) but at a cost because now
we cannot do the following:

* Cannot describe the program's logic separately from when it is
  performed/evaluated. Once you call this function a side effect is imposed
  on the operating environment. In this case we are just querying the system
  environment which may not have negative impact, but in many other cases it
  may, e.g. setting variable value in system environment.
* The return value also doesn't denote that the variable key may not exist or
  be set at all in the environment. The way that OCaml caters for this case
  is by raising an exception that unwinds the stack. i.e. producing a bottom
  value. This is not represented to the caller in the type signature.

## Usage

Usage of the respective APIs.

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
```

### OCaml

```ocaml
# getenv "HOME"
  ;;
- : string = "/home/spotter"
# getenv "HOME2"
  ;;
Exception: Not_found.
```

Ouch! We have lost the ability to encapsulate the effects and decide _when_
to perform them without the effect context in the return type (`IO` in the
Haskell/GHC case). We also don't receive a type expressing the optionality
of the result which forces _bottom_ be returned.

## Tradeoffs

There are some implementations of the equivalent `getenv` functionality
in other functional languages that return an empty `string` as an
alternative to an exception. In this case the consumer of the API loses the
ability to tell between the case of an environment variable set to the empty
string and an unset environment variable. This may be perfectly acceptable
for your needs. As the API designer you get to decide where to draw the line.

By wrapping your return values inside contexts you do require more from
the consumer to get at the "happy path" result, but you provide much more
control and/or context for the consumer of your API with additional type
safety that can help in refactoring your code base. Depending on your
programming environment and requirements you may need to weigh the memory and
CPU cycle costs of this extra wrapping. Make sure you measure whether this
impacts your system negatively before prematurely optimizing though.

The moral of the story is you must remember the limits of your ability to
reason about the code with the different API design choices available to you.

Cheers and happy reasoning.

### Thank you!

I would like to thank the follow non-functional programmers for reviewing
a rough draft of this post for flow and cohesiveness:

* [@randomfrequency](https://twitter.com/randomfrequency)
* [Peter Caswell](https://twitter.com/PeterCaswell)
