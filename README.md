scalacraft-domain
=================

A collection of case classes covering commons domains


### Design Objectives

- No dependencies outside of the platform libraries
- No binary incompatible releases
- Functional
  - Complete immutability
  - Zero exception throwing

### Constrained and Unconstrained Domain Objects

The scalacraft-domain library offers constrained instances of case classes and a parallel
collection of unconstrained case classes.

Obtaining an instance of a constrained Port,
````
import org.scalacraft.domain.net.v1._

val portOpt: Option[Port] = Port.opt(3369)
````

Obtaining an instance of an unconstrained Port,
````
import org.scalacraft.domain.net.v1.unconstrained._

val port: Port = Port(-3)
````

Constrained types have restrictions placed on the values they can be constructed with.

In the case of the constrained Port shown above the port number must be in the inclusive range [0, 65535]. In
contrast the unconstrained version of Port applies no validation to the port number.

Both variations can be used in pattern matching. The unconstrained version will match more inputs than
the constrained version.

### Implicit Conversions

Implicit views that convert between constrained and unconstrained versions of a class are included.

An unconstrained type can be converted to a option of the constrained type, while a constrained type can be
converted to an unconstrained type unconditionally.

Implicit views that convert to String are included. Additionally where the case class has one field an implicit
view of that field is provided allowing an instance to be used whether the field is required. This supports the
avoidance of direct use of primitive types while preserving the convenience. This example illustrates,

````scala
  val port = Port(6006)
  val isa = new InetSocketAddress(p)
````

### Class Overview

This sections summarise the available domain classes.

#### Net

Class | Purpose | Example
---- | ---- | ----
Port | A port number | 8080


### Documentation Notes

##### Numeric Ranges

The notation [m, n] denotes the inclusive range m <= x <= n. For example [0, 65535] refers to the range,

    0, 1, 2, ..., 65535

### Roadmap

[ ] Implicit conversion from constrained types to the corresponding unconstrained types
