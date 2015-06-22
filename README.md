scalacraft-domain
=================

[![Join the chat at https://gitter.im/janekdb/scalacraft-domain](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/janekdb/scalacraft-domain?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

A collection of case classes covering common domains

![Build Status](https://travis-ci.org/janekdb/scalacraft-domain.svg?branch=master) https://travis-ci.org/janekdb/scalacraft-domain

### API Documentation

http://janekdb.github.io/scalacraft-domain/

### Design Objectives

- Functional
- No exceptions thrown from public api
  - Except when rejecting null constructor args for unconstrained types

### Implementation Objectives

- No dependencies outside of the platform libraries
- Complete immutability
- No if statements

### Class Overview

This sections summarises the available domain classes.

#### Binary

Class | Purpose | Example
---- | ---- | ----
Octet | Integers in the range [0, 255] | 129
OctetPair | Integers in the range [0, 65535] | 0x4043

#### Country

Class | Purpose | Example
---- | ---- | ----
CountryCodeA2 | Alpha 2 Country Codes | AW
CountryCodeA3 | Alpha 3 Country Codes | ABW
CountryCodeNumeric | Numeric Country Codes | 732

#### Net

Class | Purpose | Example
---- | ---- | ----
Port | Port numbers | 8080
IPAddress | IP addresses | 192.162.0.83
DomainName | DNS names | scalacraft.com

### Maven

ScalaCraft-Domain is published to the Central Repository, so you simply have to add the appropriate dependency
to your POM:

````xml
  <dependencies>
    <dependency>
      <groupId>com.scalacraft.domain</groupId>
      <artifactId>scalacraft-domain</artifactId>
      <version>x.y.z</version>
    </dependency>
  </dependencies>
````

Replace `x.y.z` with the release you want to use. For example `2.1.0`.

### Semantic Versioning

This project uses semantic versioning. See http://semver.org/ for details.

#### Public API

The public api comprises of the classes found under com.scalacraft.domain.v<n> excluding
classes under `internal`.

#### Package Renaming Across Major Releases

When a major release is taken the package names of all classes are versioned by replacing `v<n>` with `v<n+1>`

##### Examples of Package Renaming

In release 4.y.z

````scala
package com.scalacraft.domain.v4.net
````

In release 5.y.z

````scala
package com.scalacraft.domain.v5.net
````

##### Package Renaming Rationale

The benefit arising from this renaming is the option to include different major versions of ScalaCraft Domain
in the same classloader with no conflicts,

This will work,

````
scalacraft-domain-2.1.1.jar
scalacraft-domain-3.7.1.jar
scalacraft-domain-5.0.17.jar
````

In practice this means that when the public api changes with a major release the change has no impact on existing
code if the previous major release of the library remains available.

### Constrained and Unconstrained Domain Objects

The scalacraft-domain library offers constrained instances of case classes and a parallel
collection of unconstrained case classes.

Obtaining an instance of a constrained Port,
````scala
import com.scalacraft.domain.v2.net._

val portOpt: Option[Port] = Port.opt(3369)
````

Obtaining an instance of an unconstrained Port,
````scala
import com.scalacraft.domain.v2.net.unconstrained._

val port: Port = Port(-3)
````

Constrained types have restrictions placed on the values they can be constructed with.

In the case of the constrained Port shown above the port number must be in the inclusive range [0, 65535]. In
contrast the unconstrained version of Port applies no validation to the port number.

All constraints are purely syntactic. This means that although the constrained version of CountryCodeA2 will
reject "iY" because the first character is lowercase it will accept "PP" which although correct from a formatting
perspective does not exists as a currently assigned ISO 3166-1 country code. An application can layer additional
validation onto of the validation provided by this library. Alternatively an application can validate values and
use the constrained class to represent valid values and the unconstrained class for invalid values which may be
useful for validation reporting.

Both variations can be used in pattern matching. The unconstrained version will match more inputs than
the constrained version.

#### API Rules for Unconstrained Types

There are two ways to obtain an instance of an unconstrained domain type. The first is by direct
instantiation using the public constructor. The second is through pattern matching on a string or other
type, string being the most common.

Rules 1 to 4 presented below are used to guide design choices when creating extractors for unconstrained types. By
following these rules the matchings choices will have improved consistency across different types and the matching
utility will be enhanced.

Terminology. In the following rules an alternative representation is a value of some type which is pattern
matched to. Often this will be string but other types maybe fulfil this role.

##### Rule 1 - Instances Have Alternative Representations

Every instance of the type obtained by direct constructor use must have at least one alternative
representation that will pattern match to the same constructor args.

Note: This rule led to the requirement for null constructor args to be rejected. If nulls were accepted then
it would be necessary under this rule to extract nulls from alternative representations. It would have been
possible to have a private constructor restricting object creation to the companion object. This was rejected
because the it would result in this pattern of use: `MyUnconstrained.opt(nonNull).get`.

In mathematical parlance this defines a function from a subset of all possible strings (or alternative
representations) onto the set of all possible domain type instances.

##### Rule 2 - Instance For All Alternative Representations

This rule complements Rule 1: There is no alternative representation including any string representation
that will pattern match to a list of values that cannot be used as constructor args.

Rule 1 allows that two strings could be equivalent to the same value of a type, while Rule 2 precludes the
possibility of extracting values from a representation that do not equate to a possible instance.

##### Rule 3 - Additional Freedom

Given a unconstrained domain type it must be possible to create an invalid instance given only valid constructor
args to pick from.

The purpose of Rule 3 is to ensure the type is contributing to the unconstrained nature of the type at a higher
level than the properties that comprise it. For example if the IP4Address type had a constructor that took four
integers and we had only valid octets to work with (integers in the range [0,255]) then it would be impossible
to construct an invalid IP4Address. In this case the validity of the IP4Address is implied by the validity of the
constructor args therefore IP4Address is failing to provide any way of capturing a value that is invalid at a level
beyond invalid octet values. Possible corrections in this case would be to use optional args or a variable list
of octets thereby allowing an invalid IP4Address to be constructed entirely from valid octets.

Note: IP4Address, Port and possibly others currently violate Rule 3. This is a defect requiring a breaking
release to correct to change constructor signatures from `x: T` to `x: Option[T]` which introduces the ability
to have an invalid type when only valid instances of T are available.

A consequence of Rule 3 being applied is an increase in the number of strings or alternative representations
that will pattern match.

##### Rule 4 - Maximum Loss Free Information Conversion

This rule has two parts related to the information taken when matching. The first part tells us to use all the
information we take. The second part is an directive to consume as much information as possible. These two aspects
are elaborated on in the text that follows.

###### Full Information Utilisation

When an alternative representation is matched and the extracted values are used to create an instance then
it must be possible to create an alternative representation that contains the same information as the initial
alternative representation.

For example if the string "2<3<5<7" is matched and if we regard the information that is present in this string as,

 - a set of integers
 - an ordering relationship between these integers

then both of these alternative representations include this information

 - "2<3<5<7"
 - "7>5>3>2"

whereas some of this information is lost in both of these examples

 - "2<3<5"
 - 210

Note: 210 = 2 x 3 x 5 x 7

Motivation. Given a type `Example(Option[E])` it would be possible to match every conceivable alternative
representation because `Example(None)` is always a match when the match target does not map to an allowed value
of `E`.

This rule disallows matching on information that is not retained in the matched values. A benefit of this is
the ability to have many match cases and know that an extractor will only match when it fully uses the target
data which leads to a useful chaining of match attempts.

If the first match case could match any value then later cases will never have the chance to match the data
more usefully. There is a valid comparison to parsers that fail on input streams and then backtrack to allow
the next parser to attempt a match.

Another way of looking at this is to note that all the information in the target value is still available
following the match. There is no loss of information.

A consequence of this rule is a reduction to a subset of all possible alternative representations an
unconstrained type can pattern match to.

Whitespace is not regarded as useful information so can be dropped under this rule.

###### Greedy Information Consumption

This rule directs the matcher to use as much information while not conflicting with the full information utilisation
rule.

For example if an unconstrained octet has type `Octet[Option[Int]]` then both `127` and `4000100` should be
matched despite the latter value being out of range for an octet.

### Implicit Conversions

#### Between domain types

Implicit views that convert between constrained and unconstrained versions of a class are included.

An unconstrained type can be converted to a option of the constrained type, while a constrained type can be
converted to an unconstrained type unconditionally.

#### Between domain and platform types

Implicit views that convert to String are included. Additionally where the case class has one field an implicit
view of that field is provided allowing an instance to be used whether the field is required. This supports the
avoidance of direct use of primitive types while preserving the convenience. This example illustrates,

````scala
  val port = Port(6006)
  val isa = new InetSocketAddress(p)
````

#### To field values

When a case class has a single constructor parameter an implicit view is provided that allows explicit field access
to be omitted. For example given a Port an assignment to an int will compile,

````scala
  val port = Port(6006)
  val portNumber: Int = port
````

### Documentation Notes

##### Numeric Ranges

The notation [m, n] denotes the inclusive range m <= x <= n. For example [0, 65535] refers to the range,

    0, 1, 2, ..., 65535

### Domain Roadmap

- [ ] Address: Add US zip code
- [ ] Address: Add UK postcode
- [x] Binary: Octet
- [x] Binary: OctetPair
- [x] Country: Add ISO country codes for alpha-2
- [x] Country: Add ISO country codes for alpha-3
- [x] Country: Add ISO country codes for numeric
- [x] Net: Add domain name
- [x] Net: Add ip4 address
- [ ] Net: Add ip6 address
- [x] Net: Add port
- [ ] Payment: PAN
- [ ] Payment: CVV
- [ ] Payment: Expiry date
