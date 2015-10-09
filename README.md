# Fuckops

A repository for code experiments related to writing system, cluster, network,
deployment, and application management operations tools.

## Manifesto

FuckOps (as it is today). Life is too short to waste your time hero-ing .

The guiding principles behind the `fuckops` mindset:
* **Typed:** upholding typesafety _when_ it matters.
* **API First:** Forget tools that expect you to use their CLI only, or
  worse still, UI-driven with API being on the back burner. Fuck that.
* **Bootstrappable:** This restates the essence of the 'Infrastructure as
  Code' mantra. Current Ops practices demonstrated via mainstream approaches
  today shows it doesn't believe in this. Not in reality. Annoying as fuck.
* **Testable:** It's amazing how many Ops tools are not really testable at all
  without manual actions involved. Life is too short.
* **Well Defined:** Use well defined (and simple) APIs and protocols to make
  this happen.

## Getting Started

1.  Install [Haskell Platform](http://www.haskell.org/platform/) &gt;=2013.02.0.0.
2.  Drink excessive amounts of coffee while you wait and stop whining. Think of
    all the time you will be saving later.
3.  Run: `cabal configure --enable-tests`.
4.  Run: `cabal install --dependencies-only`.
5.  Run: `cabal build`.
6.  Open vim (or emacs even, if you must).
7.  Tinker.
8.  Contribute, if you want.
9.  Drink more coffee.
10. FuckOps.
11. Rinse.
12. Repeat.

## Contribute

1. Fork this repo.
2. Create a well named branch off of the current master (`typedops/fuckops`).
3. Work on your changes.
4. Push your changes to your fork.
5. Submit a pull request from your fork to this repo and provide a description of your changes.

Go forth and FuckOps.

Cheers.
