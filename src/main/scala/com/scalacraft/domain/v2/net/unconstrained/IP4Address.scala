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
package com.scalacraft.domain.v2.net.unconstrained

import com.scalacraft.domain.v2.internal.NumericConversions.FromString
import com.scalacraft.domain.v2.net.{IP4Address => ConstrainedIP4Address}

import scala.util.control.Exception._
import scala.util.matching.Regex

/**
 * An `IP4Address` represents an IP4 address.
 *
 * This class does not constrain the value of the address components.
 *
 * === Pattern Matching ===
 *
 * Pattern matching is supported as the following example demonstrates,
 * {{{
 * "192.162.0.9" match {
 * case IP4Address(b1, b2, _, _) => Some(b1, b2) // Some(192, 162)
 * case _ => None
 * }
 * }}}
 *
 * === Implicit Conversions ===
 *
 * Implicit conversions exist which allow an instance of `IP4Address` when a `String` is required.
 *
 * {{{
 * val ipa = IP4Address(b1, b2, b3, b4)
 * val inet = java.v2.InetAddress.getByName(ipa)
 * }}}
 *
 * A conversion to an option of the constrained version of this class is also available.
 */
case class IP4Address(byte1: Int, byte2: Int, byte3: Int, byte4: Int) {
  private def tuple = (byte1, byte2, byte3, byte4)
}

object IP4Address {

  /**
   * Provide a string representation for an instance of this class
   * @param ip4Address The instance to use.
   * @return A dotted quad. For example "192.168.0.23"
   */
  implicit def `to-String`(ip4Address: IP4Address): String =
    ip4Address.tuple.productIterator.mkString(".")

  implicit def `to-Option[IP4Address]`(ip4Address: IP4Address): Option[ConstrainedIP4Address] =
    ConstrainedIP4Address.opt(ip4Address.byte1, ip4Address.byte2, ip4Address.byte3, ip4Address.byte4)

  private val DottedQuadPat = {
    val digits = "(-?[\\d]++)"
    val dot = "\\."
    ((digits + dot) * 3 + digits).r
  }

  private def opt(quad: String): Option[IP4Address] = quad match {
    case DottedQuadPat(s1, s2, s3, s4) =>
      for {
        b1 <- s1.optInt
        b2 <- s2.optInt
        b3 <- s3.optInt
        b4 <- s4.optInt
      } yield IP4Address(b1, b2, b3, b4)

    case _ => None
  }

  def unapply(x: String): Option[(Int, Int, Int, Int)] =
    opt(x) map (_.tuple)

}