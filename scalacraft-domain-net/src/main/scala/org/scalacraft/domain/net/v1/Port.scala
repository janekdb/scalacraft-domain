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
package org.scalacraft.domain.net.v1

import scala.util.control.Exception.catching

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
 * When any class constraint would be violated the result is `None`.
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
 *   val port = Port.opt(6006)
 *   val sa = port map {
 *     p => new InetSocketAddress(p)
 *   }
 * }}}
 */
case class Port private(portNumber: Int)

object Port {

  implicit def `to-Int`(port: Port): Int = port.portNumber

  implicit def `to-String`(port: Port): String = port.portNumber.toString

  implicit def `to-Port`(port: Port): unconstrained.Port = unconstrained.Port(port.portNumber)

  private implicit class sx(val s: String) {
    def optInt = catching(classOf[NumberFormatException]) opt s.toInt
  }

  private val inRange = Range(0, 65535 + 1).contains _

  def opt(portNumber: Int): Option[Port] =
    Some(portNumber) filter (inRange) map (Port(_))

  def opt(portNumber: String): Option[Port] =
    portNumber.optInt filter (inRange) map (Port(_))

  def unapply(x: Int): Option[Int] =
    Some(x) filter (inRange)

  def unapply(x: String): Option[Int] =
    x.optInt filter (inRange)
}