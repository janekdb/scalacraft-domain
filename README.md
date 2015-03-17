scalacraft-domain
=================

A collection of case classes covering common domains

![Build Status](https://travis-ci.org/janekdb/scalacraft-domain.svg?branch=master) https://travis-ci.org/janekdb/scalacraft-domain

### Design Objectives

- No dependencies outside of the platform libraries
- Functional
  - Complete immutability
  - No exceptions thrown from public api
- No if statements

### Class Overview

This sections summarise the available domain classes.

#### Country

Class | Purpose | Example
---- | ---- | ----
CountryCodeA2 | Alpha 2 Country Codes | AW
CountryCodeA3 | Alpha 3 Country Codes | ABW
CountryCodeNumeric | Numeric Country Codes | 732

#### Net

Class | Purpose | Example
---- | ---- | ----
Port | A port number | 8080
IPAddress | An IP address | 192.162.0.83
DomainName | A DNS name | scalacraft.com

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
- [ ] Binary: Octet
- [ ] Binary: OctetPair
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
