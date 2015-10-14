---
layout: post
title: Why Use Types?
date: 2015-10-09 05:50 -0500
category: tutorials
tags:
  - valueclasses
  - option
  - products
  - coproducts
---

Types are one of the most mainstream and widespread applied formal methods
in software engineering industry today. Yet I have met many accomplished and
experienced practitioners in our field who do not know how to effectively use
types, even in lanuages they can leverage well for this.

This tutorial is an attempt to show step by step an iterative approach to
transforming what I would consider an ill-typed API (a form I see very often
in operations or infrastructure code) towards a more type-expressive API
which eleminates a large set of invalid values from ever being constructed in
the first place.

This will be the basis for future tutorials that will follow, putting into
practice more structural typed techniques often used in typed functional
programming. My claim here is that pairing a half decent type system that
can express many valid constructions of values (and eliminate many invalid
constructions) with functional programming principles and abstractions we can
build a solid core of a working library or executable. It will allow us to
reason about our code such that we can extend it in ways we previously thought
unimaginable without much heavy lifting (after the initial plumbing work). We
will start to see evidence for this claim in this tutorial post and subsequent
tutorials will build upon this foundation to supply more evidence as we go
along on our journey.

## To The Cloud And Beyond

Let's start out with a simple example. We need to model VPCs and Subnets in
AWS:

```tut
case class Vpc(
  cidrBlock: String,
  region: String,
  dhcpOptionsId: String,
  instanceTenancy: String,
  isDefault: Boolean)

case class Subnet(
  cidrBlock: String,
  vpcId: String,
  availabilityZone: String,
  defaultForAz: Boolean,
  mapPublicOnLaunch: Boolean)
```

Now let's use these definition to construct values:

```tut
val region = "us-west-14"
val vpc = Vpc("10.0.0.0/16", region, null, "dedicated", false)
val subnet = Subnet("10.10.16.0/24", "sg-123456", s"${region}b", false, false)
```

There are a few problems in just this simple usage of these simple classes:

1. There is no region named `us-west-14` and the derived availability zone
   value is also invalid.
2. A `null` was given for `dhcpOptionsId` argument but the behavior of various
   utilities using these types is unknown when this happens as we don't know
   if they will actually handle `null`'s properly with defensive programming.
3. We might have fat fingered the `instanceTenancy`, this time we didn't, but
   who knows when everything is just of type `String`.
4. We have no idea if the given `cidr` block arguments to either `Vpc` or
   `Subnet` are valid since we just expect a `String` representing a CIDR.
5. We have no idea if the string id for DHCP options or VPC are referring to
   identifiers of the right kind or not.

We started with such a simple API and already have numerous issues with just
using weak types in our data type definitions.

Now let's toughen up these types.

```tut
// We build a sum type (also called coproduct in some literature)
// This is a kind of algebraic data type that represents a logical OR
sealed trait Region
case object UsWest1 extends Region
case object UsWest2 extends Region
case object UsEast1 extends Region
case object EuWest1 extends Region

// Another sum type here for zone *inside* of a region
sealed trait Zone
case object A extends Zone
case object B extends Zone
case object C extends Zone

// This is a product type (a dressed up N-tuple)
// This is another kind of algebraic data type representing logical AND
case class AvailabilityZone(region: Region, zone: Zone)

// A sum type to represent the notion of instance tenancy, I am sure I am
// missing possible data constructors (the case objects/classes that extend
// from the base class, in this case +InstanceTenancy+).
sealed trait InstanceTenancy
case object Dedicated extends InstanceTenancy
case object Default extends InstanceTenancy

case class Vpc(
  cidrBlock: String,
  region: Region,
  dhcpOptionsId: Option[String],
  instanceTenancy: InstanceTenancy,
  isDefault: Boolean)

case class Subnet(
  cidrBlock: String,
  vpcId: String,
  availabilityZone: AvailabilityZone,
  defaultForAz: Boolean,
  mapPublicOnLaunch: Boolean)

```

Now we have some provisioning logic that picks the appropriate AWS
credentials when using specific regions vs others:

```tut
// we are just mocking out this function for now and not *doing* anything yet
def provisionVpc(vpc: Vpc): String =
  if (vpc.region == "us-west-1") "vpc-123456" else "vpc-654321"
```

Here we have a problem. The `==` is always false, but shouldn't it be a
compiler error instead? In Scala, `==` takes on the same semantics as
the same operator in Java and thus will always compile even when you
are comparing the value of a different type.

Thankfully in libraries like `cats` or `scalaz` (we will be using
`cats` in these examples) we can use a type safe equality operator
like so:

```tut

import cats._, cats.std.all._, cats.syntax.all._

implicit val regionEquality = new Eq[Region] {
  def eqv(r1: Region, r2: Region): Boolean = r1 == r2
}
```

It may not seem like we have accomplished much here except that when we use
`===` (the typesafe equality operator) our code will not compile when we
check different types for equality:

```tut:fail
// Now reimplement provisionVpc (but this does not compile, which is what we
// want!!!)
def provisionVpc(vpc: Vpc): String =
  if (vpc.region === "us-west-1") "vpc-123456" else "vpc-654321"

```

The following will compile and is exactly what we want in our case:

```tut
// Now reimplement provisionVpc where types on either side of === match such
// that this now compiles but also the logic behaves as we expect and can
// reason about.
def provisionVpc(vpc: Vpc): String =
  if (vpc.region === UsWest1) "vpc-123456" else "vpc-654321"
```

Now we could still improve the constituent types of the `Vpc` and `Subnet`
product types further such as accepting a `Cidr` type for the CIDR block
given:

```tut
import spire.math._, spire.implicits._

// Note: UByte is from spire. It is the most natural type that represents
// what we want below without defining a low level type ourselves.
// Let me know if I missed something from the standard library that isn't
// Byte.

sealed trait IpAddress
object IpAddress {
  private case class Ipv4(a: UByte, b: UByte, c: UByte, d: UByte) extends IpAddress
  // TODO: implement Ipv6 when needed

  def apply(a: UByte, b: UByte, c: UByte, d: UByte): IpAddress =
    Ipv4(a, b, c, d)
}

// We want an unsigned byte to represent the Prefix (at least for now)
type Prefix = UByte
def prefix(p: UByte): Option[Prefix] =
  if (p <= UByte(32) && p >= UByte(0)) Some(p) else None

case class Cidr(ip: IpAddress, prefix: Prefix)
```

So far we have addressed 1 through 4 of the list of problems we identified
above. Let's recap what we did:

1. We encoded the notion of region as a sum type and changed the type used
   from `String` to `Region`. We have eliminated a large number of possible
   invalid values from being used in it's place such that non-existent AWS
   regions referenced would not even compile.
2. We encoded in our type signature for the `Vpc` product type (case class)
   that we do not require DHCP options identifier to construct a valid `Vpc`
   value. This will inform implementers of functions that use this value that
   this field is an optional value so they can plan accordingly.
3. We encoded the notion of instance tenancy as a sum type and switched from
   using a `String` to represent the possible values of it to this new type,
   `InstanceTenancy`. The compiler will not catch any fat fingering we may
   have made. Think of the potential troubleshooting and debugging time we
   gained back from this simple act.
4. We have encoded a more meaningful representation for CIDR values that will
   limit the possible values to valid basic constructions. A `String` value
   gives us know structure to verify and the validation logic on a `String`
   value for the CIDR case would be error prone and complex without decomposing
   into the structural elements of the value in the first place. Even using
   regular expressions to validate a `String` representing a CIDR requires us
   to decompose the value into its elements (plus it might not be very
   efficient but that is a secondary concern). :)

Now we can tackle referential consistency of our string ids via a construct
in Scala called value classes. This allows us to enforce type safety at
compile-time without the runtime allocation overhead.

For our purposes we will wrap up `String` identifiers designating what kind
of identifier it refers to. As an example whenever we provision or query VPCs
we can wrap the basic string identifier that AWS returns back as a specific
type `VpcId`. This will not allocate a boxed value but will uniquely identify
the type of `String` at compile-time.

```tut
class VpcId (private val id: String) extends AnyVal
class DhcpOptionsId private (val id: String) extends AnyVal

case class Vpc(
  cidrBlock: String,
  region: Region,
  dhcpOptionsId: Option[DhcpOptionsId],
  instanceTenancy: InstanceTenancy,
  isDefault: Boolean)

case class Subnet(
  cidrBlock: String,
  vpcId: VpcId,
  availabilityZone: AvailabilityZone,
  defaultForAz: Boolean,
  mapPublicOnLaunch: Boolean)
```

We also need to make sure the functions provisioning or querying and returning
these identifiers return the appropriate wrapper values.

```tut
// Note we are returning VpcId not String any more
def provisionVpc(vpc: Vpc): VpcId = ???

def defaultDhcpOptions(region: Region): DhcpOptionsId = ???
```

Now we will be able to see from the type signature something is amiss if we
are returning a `VpcId` from a function named `defaultDhcpOptions`. Obviously
that isn't the level of sanity checking we are aiming for, but it's a reason-
able start for this session.

You'll note that we haven't addressed the problem (yet) that we can simply
construct invalid `VpcId` values by passing in illegal format of `String`
value to the value class constructor. This will be addressed by a couple of
different techniques in future tutorials. Just know that we will be returning
to this. Promise. :)

We could also replace the type alias for `Prefix` with a value class:

```tut

class Prefix (val b: Byte) extends AnyVal
object Prefix {
  private def isValid(b: UByte): Boolean =
    (b <= UByte(32) && b >= UByte(0))

  def apply(b: UByte): Option[Prefix] =
    if (isValid(b)) Some(new Prefix(b.toByte)) else None
}
```

The purpose of this initial tutorial was to introduce the reader to the
utility of types generally and specifically how to start to make your APIs
more type safe in Scala using non-structural techniques.

Hopefully we can take away a sense of purpose in types. They don't have to be
source code annotations that just add more code bloat, they can add
enormous _value_ (yes, I couldn't resist the pun). You might also have noted
that we have gone quite far with types, yet we can still do more to limit
the values we construt in our code to be only valid ones. How far can we take
this? How far makes sense? Keep these questions in mind.

The next tutorial in the series will walk through how we add some structure
to our types with accompanying functions with specific properties we can
use to reason about our code in more abstract ways.
