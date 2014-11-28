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
val portOpt: Option[Port] = Port.opt(3369)
````

Constrained types have certain limitations placed on the values they can be constructed with.

In the case of Port shown above the port number must be in the inclusive range [0, 65535]. In contrast
the unconstrained version of Port applies no validation to the port number.

Both variations can be used in pattern matching. The unconstrained version will match more inputs than
the constrained version.

### Roadmap

[ ] Implicit conversion from constrained types to the corresponding unconstrained types
