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
package com.scalacraft.domain.v2.net

import com.scalacraft.domain.v2.internal.NumericConversions.FromString

/**
 * A `Port` represents an IP port.
 *
 * The following constraints hold for instances of this class,
 *
 * - `portNumber` is in the range [0, 65535]
 *
 * An instance can be created using a suitable overload of the `opt` method.
 *
 * {{{
 *   val portOpt: Option[Port] = Port.opt(1908)
 *   val portOpt2: Option[Port] = Port.opt("40115")
 * }}}
 *
 * When any class constraint is violated the result is `None`.
 *
 * === Pattern Matching ===
 *
 * Pattern matching is supported as the following examples demonstrate,
 * {{{
 *   7 match {
 *     case Port(p) => p // 7
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
 * Invalid ports are not matched,
 * {{{
 *   -129 match {
 *     case Port(p) => p
 *     case _ => None // None
 *   }
 * }}}
 *
 * === Implicit Conversions ===
 *
 * Implicit conversions exists which allow an instance of `Port` to be used when an `Int` or `String` is required.
 *
 * {{{
 *   val port = Port.opt(6006)
 *   val sa = port map {
 *     p => new InetSocketAddress(p)
 *   }
 * }}}
 *
 * A conversion to the unconstrained version of this class is also available.
 */
case class Port private(portNumber: Int)

object Port {

  @deprecated(since = "2.1.0")
  implicit def `to-Int`(port: Port): Int = port.portNumber

  @deprecated(since = "2.1.0")
  implicit def `to-String`(port: Port): String = port.portNumber.toString

  implicit def `to-Port`(port: Port): unconstrained.Port = unconstrained.Port(port.portNumber)

  private val inRange = Range(0, 65535 + 1).contains _

  def opt(portNumber: Int): Option[Port] =
    Some(portNumber) filter inRange map (Port(_))

  def opt(portNumber: String): Option[Port] =
    portNumber.optInt filter inRange map (Port(_))

  def unapply(x: Int): Option[Int] =
    Some(x) filter inRange

  def unapply(x: String): Option[Int] =
    x.optInt filter inRange
}