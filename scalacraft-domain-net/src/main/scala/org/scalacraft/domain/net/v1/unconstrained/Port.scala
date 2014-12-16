/*
   Copyright 2014 Janek Bogucki

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
package org.scalacraft.domain.net.v1.unconstrained

import scala.util.control.Exception._

import org.scalacraft.domain.internal.NumericConversions.FromString
import org.scalacraft.domain.net.v1.{Port => ConstrainedPort}

/**
 * A `Port` represents an IP port.
 *
 * This class does not constrain the value of the port in anyway.
 *
 * Pattern matching is supported as the following examples demonstrate,
 * {{{
 *   7 match {
 *     case Port(p) => p
 *     case _ => None
 *   }
 * }}}
 *
 * The match target can be a string,
 * {{{
 * val s: String = ...
 *   s match {
 *     case Port(p) => p
 *     case _ => None
 *   }
 * }}}
 *
 * Implicit conversions exists which allow an instance of `Port` to be used when an `Int` or `String` is required.
 *
 * {{{
 *   val port = Port(6006)
 *   val isa = new InetSocketAddress(p)
 * }}}
 *
 * A conversion to an option of the constrained version of this class is also available.
 */
case class Port(portNumber: Int)

object Port {

  implicit def `to-Int`(port: Port): Int = port.portNumber

  implicit def `to-String`(port: Port): String = port.portNumber.toString

  implicit def `to-Option[Port]`(port: Port): Option[ConstrainedPort] = ConstrainedPort.opt(port.portNumber)

  def unapply(x: Int): Option[Int] = Some(x)

  def unapply(x: String): Option[Int] = x.optInt
}