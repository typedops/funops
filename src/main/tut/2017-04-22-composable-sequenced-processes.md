---
layout: post
title: 'Composable Sequenced Processes'
date: 2017-04-22 21:07 -0500
category: tutorials
tags:
  - types
  - referential transparency
  - processes
  - sequencing
---

This blog series demonstrates how typed functional
programming is relevant in operations and why you should care using simple,
yet practical and motivating examples. This post will walk through writing
automation for secrets management: we will verify a signature of an encrypted
document, decrypt the document, verify a signature for the decrypted document.

For those interested here is an article on [the simple sign & encrypt
problem](http://world.std.com/~dtd/sign_encrypt/sign_encrypt7.html).

These are sequenced steps that get automated in CI/CD pipelines, or
app/service initialization frequently.

Let's say we have some Javascript (don't ask, just go with it) that you
inherited which has the following skeleton and structure:

```javascript

// Fabricated so we can focus on the essence of the problem.
// Note: I don't know ES6 yet so I stuck with the kind of Javascript
// I am familiar with when I used to write it c2004. I also didn't
// want language features to distract from the design discussion.
// Let me know if it does anyway.

// A provider implementation is passed in so that we can
// control the expected interface via the Crypto0 constant.
const Crypto0 = function (provider) {
  var that = this;

  that.verifyDoc = function (signature, doc) {
    return provider.verifyDoc(signature, doc);
  };

  that.decryptDoc = function (doc) {
    return provider.decryptDoc(doc);
  };

  that.encryptDoc = function (doc) {
    return provider.encryptDoc(doc);
  };

  return that;
};

// don't recommend for use in performance critical code :)
function reverse(s) {
  return s.split('').reverse().join('');
}

/*
  @param secrets object literal of object literals looking like:
  { encrypted: "....", encSig: "....", decSig: "...." }

  For each encrypted document content passed in:
  (a) verify the encrypted doc with the external signature
  (b) decrypt the encrypted content
  (c) verify the decrypted doc contents with the unencrypted signature

  If any of the above steps (per document) fail, signify an error occurred
  to the client somehow. Otherwise return an object literal with keys for
  each <file> and associated values for the secrets.
*/
function decryptSecrets(secrets, provider) {
  var crypto = new Crypto0(provider);
  return secrets.map(function (x, idx) {
    if (crypto.verifyDoc(x.encSig, x.encrypted)) {
      var decrypted = crypto.decryptDoc(x.encrypted);
      if (crypto.verifyDoc(x.decSig, decrypted)) {
        return { encrypted: x.encrypted, encSig: x.encSig, decSig: x.decSig, decrypted: decrypted };
      } else {
        return new Error("decrypted signature cannot be verified for " + idx);
      }
    } else {
      return new Error("encrypted signature cannot be verified for " + idx);
    }
  });
}
```

Here is a mocked up scenario to run and see the output:

```javascript
const MockSuccessProvider0 = function () {
  var that = this;
  that.verifyDoc = function (sig, doc) { return true; };
  that.encryptDoc = function (doc) { return reverse(doc); };
  that.decryptDoc = function (doc) { return reverse(doc); };
  return that;
};

const MockFailureProvider0 = function () {
  var that = this;
  that.verifyDoc = function (sig, doc) { return false; };
  that.encryptDoc = function (doc) { return reverse(doc); };
  that.decryptDoc = function (doc) { return reverse(doc); };
  return that;
};

function examples(f) {

  console.log(f([
    {
      encrypted: "oy oy oy",
      encSig: "bla",
      decSig: "bla"
    },
    {
      encrypted: "rab oof",
      encSig: "baz",
      decSig: "baz"
    }
  ], new MockSuccessProvider0()));

  console.log(f([
    {
      encrypted: "oy oy oy",
      encSig: "bla",
      decSig: "bla"
    },
    {
      encrypted: "rab oof",
      encSig: "baz",
      decSig: "baz"
    }
  ], new MockFailureProvider0()));

}


```

The output of this mocked up scenario is:


```
> examples(decryptSecrets);

[ { encrypted: 'oy oy oy',
    encSig: 'bla',
    decSig: 'bla',
    decrypted: 'yo yo yo' },
  { encrypted: 'rab oof',
    encSig: 'baz',
    decSig: 'baz',
    decrypted: 'foo bar' } ]
[ [Error: encrypted signature cannot be verified for 0],
  [Error: encrypted signature cannot be verified for 1] ]
```

One thing to notice is that the above has no side effects (except the logging
in the usage at the end to demonstrate it working). This allows us to focus on
the core parts of the problem, pushing out system interaction and side effects
to outer layers, and making the code usable in more contexts.

We inject the crypto provider as an argument which means
we can swap out providers depending on the running environment. For example
when running in Node we can use whatever Node package we wish to use to provide
encryption and signature verification functionality, whereas in the browser
we can wrap browser cryptographic APIs in the `Crypto0` interface. When
testing (like above with our mock providers) we can define the providers
we need to satisfy our test cases. No dangerous stubs or test doubles that
modify other object behaviors at runtime just to test logic contained elsewhere.
It uses just simple mock providers, which are passed in at the call-site in
our consumer code.

Similarly we have the benefit that the `decryptSecrets` function accepts
values for the encrypted and signature contents instead of passing in paths
or URLs or DOM ids since depending on the our running context we may wish to
implement this differently.

In the browser we might want to choose between retrieving encrypted and
signature content from DOM elements referenced by IDs or retrieving the
content by URLs. In a Node running environment we may choose either file
paths or network URLs.

It's nice and all that we inherited a referentially transparent
core and I appreciate that we can inject our crypto provider and determine
how to read in the encrypted data and signatures from the consuming code,
but is it awesome yet? The function used to map over the given secrets is a
little clunky. What if we needed it to provide slightly differing logic?

For that matter, what if we'd like to make verifying secrets on both sides
optional? As in whether to verify the encrypted data before decrypting or
whether to verify the decrypted data at the end. We'd have to maintain three
new versions of the function with little or no ability to meaningfully reuse
existing definitions or structure.

Let's see how we might be able to improve our code to offer a more composable
interface. Specifically we want to allow verifying signatures on either
encrypted or decrypted sides optional by just composing functions together.

## Unify and contextualize your sequencing

Let's start off with the dumbest solution to our new requirements of
implementing a different function for each possible case.

Below we rename the original `decryptSecrets` function to describe
the context in which it operates (namely that it will verify both the encrypted
and decrypted signatures or _fail_ otherwise):

```javascript
function decryptSecretsWithFullVerification(secrets, provider) {
  var crypto = new Crypto0(provider);
  return secrets.map(function (x, idx) {
    if (crypto.verifyDoc(x.encSig, x.encrypted)) {
      var decrypted = crypto.decryptDoc(x.encrypted);
      if (crypto.verifyDoc(x.decSig, decrypted)) {
        return { encrypted: x.encrypted, encSig: x.encSig, decSig: x.decSig, decrypted: decrypted };
      } else {
        return new Error("decrypted signature cannot be verified for " + idx);
      }
    } else {
      return new Error("encrypted signature cannot be verified for " + idx);
    }
  });
}
```

The function works the same way, but we've renamed it.

Below we define the other functions named according to how they behave:

```javascript
function decryptSecretsWithEncVerification(secrets, provider) {
  var crypto = new Crypto0(provider);
  return secrets.map(function (x, idx) {
    if (crypto.verifyDoc(x.encSig, x.encrypted)) {
      var decrypted = crypto.decryptDoc(x.encrypted);
      return { encrypted: x.encrypted, encSig: x.encSig, decSig: x.decSig, decrypted: decrypted };
    } else {
      return new Error("encrypted signature cannot be verified for " + idx);
    }
  });
}

function decryptSecretsWithDecVerification(secrets, provider) {
  var crypto = new Crypto0(provider);
  return secrets.map(function (x, idx) {
    var decrypted = crypto.decryptDoc(x.encrypted);
    if (crypto.verifyDoc(x.decSig, decrypted)) {
      return { encrypted: x.encrypted, encSig: x.encSig, decSig: x.decSig, decrypted: decrypted };
    } else {
      return new Error("decrypted signature cannot be verified for " + idx);
    }
  });
}

function decryptSecretsWithNoVerification(secrets, provider) {
  var crypto = new Crypto0(provider);
  return secrets.map(function (x, idx) {
    var decrypted = crypto.decryptDoc(x.encrypted);
    return { encrypted: x.encrypted, encSig: x.encSig, decSig: x.decSig, decrypted: decrypted };
  });
}
```

Take a close look at the definitions of these functions. There is much
commonality yet not in a way we can reuse parts by combining them simply, yet.

To check the behavior is as we would expect for the two examples we
already provided above, let's still use the mock providers via the `examples`
helper function above:

```

> examples(decryptSecretsWithEncVerification);
[ { encrypted: 'oy oy oy',
    encSig: 'bla',
    decSig: 'bla',
    decrypted: 'yo yo yo' },
  { encrypted: 'rab oof',
    encSig: 'baz',
    decSig: 'baz',
    decrypted: 'foo bar' } ]
[ [Error: encrypted signature cannot be verified for 0],
  [Error: encrypted signature cannot be verified for 1] ]

> examples(decryptSecretsWithDecVerification);
[ { encrypted: 'oy oy oy',
    encSig: 'bla',
    decSig: 'bla',
    decrypted: 'yo yo yo' },
  { encrypted: 'rab oof',
    encSig: 'baz',
    decSig: 'baz',
    decrypted: 'foo bar' } ]
[ [Error: decrypted signature cannot be verified for 0],
  [Error: decrypted signature cannot be verified for 1] ]

> examples(decryptSecretsWithNoVerification);
[ { encrypted: 'oy oy oy',
    encSig: 'bla',
    decSig: 'bla',
    decrypted: 'yo yo yo' },
  { encrypted: 'rab oof',
    encSig: 'baz',
    decSig: 'baz',
    decrypted: 'foo bar' } ]
[ { encrypted: 'oy oy oy',
    encSig: 'bla',
    decSig: 'bla',
    decrypted: 'yo yo yo' },
  { encrypted: 'rab oof',
    encSig: 'baz',
    decSig: 'baz',
    decrypted: 'foo bar' } ]

```

In addition to the original implementation which we renamed to
`decryptSecretsWithFullVerification` we had to create three more functions
to handle all the cases for the dumbest solution possible.

SHIP IT! Right?

Maybe we should hold up before we do. While we have something that is
[almost] shippable from a core functionality perspective - we still need to
glue all these things in, which I left as an exercise for the reader because
the thought of doing so made me feel queasy - but this is not the kind of
code we want to maintain going forward, is it? How about if we need to
extend it further? We either provide a unifying interface for consumers
of our API or we push the complexity into the consuming codebase.

Ideally we want to be able to pass some context into the original
`decryptSecrets` function that hides the specific implementation our consumer
requires. This context dictates how we sequence the steps in our `Crypto0`
API in our _programs_. This will eliminate the newly created need to glue all
these varieties of the `decryptSecretsXXXX` functionality in our dumb
solution above in the consuming code.

So how will we pass in this context if we just offer a unified interface
for the core operations of `verifyDoc`, `decryptDoc`, and `encryptDoc`?
(We might also want to add the ability to produce signatures of documents
for completeness.)

We could wrap document encryption/decryption/signing/verifying computations
or logic (with optionally the needed signatures) passed into
the core API. If we wrapped the core values this way could we construct
new program logic by simple combinations (e.g. appending, simple sequencing
into, etc.) from smaller decrypt/encrypt/verify/sign units? Let's see.

If we have wrapped values, we will probably need to offer a way to
_unwrap_ the underlying result (so far) and apply a given function. Not only
would this be needed for sequencing purposes (before we compute step B
we must have the result of step A first) but building APIs with symmetry
is usually a good idea (unless you have a reason to break this rule).

```javascript
/*
 * We are given a wrapped program that provides the context of our sequenced
 * computation. We can extend the functionality offered by the wrapped
 * program by performing a new step that requires the result of evaluating
 * the wrapped program.
 */
function extendProgram(wrappedProgram, dependentStep) {
  dependentStep(unwrap(wrappedProgram));
}
```

Another trivial way we could extend building a program in this context
is by just requiring a sequence happened before the next step and
ignoring the evaluated value from the prior sequence. It might look
something like this:

```javascript
function appendToProgram(wrappedProgram, nextStep) {
  var ignored = unwrap(wrappedProgram);
  nextStep();
}

```

Of course, we also need to wrap simple values in the necessary context.
This might look like the following:

```javascript
/* wrap is a function that decorates the value, v. */
function wrapInContext(v, wrap) { wrap(v); }
```

How do we signify a failure condition during the execution of our program?
Here's one attempt (it's not gorgeous):

```javascript
function failInContext(message) { return Error(message); }
```

## Building the wrapped Crypto API

How could we use the above ideas to build smaller units of programs and
combine them in different ways as long as the output of the prior steps
aligns with the input for the next step?

```javascript

const Crypto1 = function (provider, wrap) {
  var that = this;

  // verifyDoc takes a secret object which has the doc, and its signature
  // whether encrypted or decrypted, returning a boolean to indicate if
  // the document could be verified against its signature.
  that.verifyDoc = function (secret) {
    return wrapInContext(provider.verifyDoc(secret.sig, secret.doc), wrap);
  };

  // decryptDoc takes a document (assumed encrypted) and decrypts it,
  // returning a decrypted document.
  that.decryptDoc = function (doc) {
    return wrapInContext(provider.decryptDoc(doc), wrap);
  };

  // encryptDoc takes a document (assumed decrypted) and encrypts it,
  // returning an encrypted document.
  that.encryptDoc = function (doc) {
    return wrapInContext(provider.encryptDoc(doc), wrap);
  };

  // adding new signing API: signDoc takes a document (either encrypted or
  // decrypted) and returns its [detached] signature.
  that.signDoc = function (doc) {
    return wrapInContext(provider.signDoc(doc), wrap);
  };
  return that;
};

```

It is starting get a little hairy and modern Javascript developers might be
wondering, "isn't this a little like Promises?" :) :) :)

Yes, in fact, it was entirely made for this purpose. Imagine that. :)

So how might it look if we wrapped our provider API with Promises?

One attempt might look like the following:

```
crypto.verifyDoc((sig, doc) => {
  crypto.decryptDoc(doc, decDec => {
    console.log(decDoc);
  }
});
```

It so happens that this is the same as:

```
crypto.verifyDoc(sig, doc)
  .then(doc => crypto.decryptDoc(doc))
  .then(decDec => console.log(decDoc));
```

which is the same as:

```
crypto.verifyDoc(sig, doc)
  .then(crypto.decryptDoc)
  .then(console.log);
```

which also happens to be the the same as:

```
(async (sig, doc) => {
  const verified = await verifyDoc(sig, doc);
  if (verified) {
    const decDoc = await decryptDoc(doc);
    await console.log(decDoc);
  }
})(sig, doc);
```

Now the longer the chains get, the harder it is going to be to keep the
passing of the output of one step into the next step as input straight
in your head. Also if you want to construct a sequential process to
perform later, then with `Promise`es you need to wrap the process itself
in another `Promise`. The above form offers the encasing layer already
with the expectation that to perform the action we call some kind of
evaluate/perform interpretator function (left as an exercise for the reader).

Now next time we will look at building the secrets management for CI/CD
in a language that can guide our brains through the pipeline transformations
at each stage using a relatively cheap mechanism (static types, welp) as
well as naturally offer such a construction for defining *composable
sequential processes*. We will do this in Purescript! :)
