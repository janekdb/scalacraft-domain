/*
   Copyright 2014 - 2015 Janek Bogucki

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
package com.scalacraft.domain.v2.binary

import com.scalacraft.domain.v2.binary.unconstrained.{Octet => Unconstrained}
import com.scalacraft.domain.v2.internal.NumericConversions.FromString

/**
 * An `Octet` represents an integer in the range [0, 255].
 *
 * The following constraints hold for instances of this class,
 *
 * - `octet` is in the range [0, 255]
 *
 * An instance can be created using a suitable overload of the `opt` method.
 *
 * {{{
 *   val octetOpt1: Option[Octet] = Octet.opt(117)
 *   val octetOpt2: Option[Octet] = Octet.opt(0x75)
 *   val octetOpt3: Option[Octet] = Octet.opt("fe") // Decimal 254
 *   val octetOpt4: Option[Octet] = Octet.opt("d") // Decimal 13
 *   val octetOpt5: Option[Octet] = Octet.opt("12") // Decimal 18
 * }}}
 *
 * Note that the string variant takes either one or two hex characters.
 *
 * When any class constraint is violated the result is `None`.
 * {{{
 *   val octetOpt: Option[Octet] = Octet.opt("NaN") // None
 * }}}
 *
 * === Pattern Matching ===
 *
 * Pattern matching is supported as the following examples demonstrate,
 * {{{
 *   7 match {
 *     case Octet(n) => n // 7
 *     case _ => None
 *   }
 * }}}
 *
 * The match target can be a string,
 * {{{
 * val s: String = "20"
 *
 * s match {
 *   case Octet(n) => n  // 32
 *   case _ => None
 * }
 * }}}
 *
 * Invalid octets are not matched,
 * {{{
 *   -129 match {
 *     case Octet(n) => n
 *     case _ => None // None
 *   }
 * }}}
 *
 * === Implicit Conversions ===
 *
 * Implicit conversions exists which allow an instance of `Octet` to be used when an `Int` or `String` is
 * required.
 *
 * {{{
 *   val Some(hi) = Octet.opt(4)
 *   val Some(lo) = Octet.opt(3)
 *   val w: Int = 256 * hi + lo // 0x0403
 * }}}
 *
 * A conversion to the unconstrained version of this class is also available.
 *
 * @param octet A valid octet
 */
case class Octet private(octet: Int) {

  /**
   * Convert to the unconstrained version of octet.
   * @return An unconstrained instance of octet
   */
  def unconstrained: Unconstrained = Unconstrained(octet)
}

// TODO: Add trait to allow merge with Port following CountryCode regex example
object Octet {

  @deprecated(since = "2.1.0")
  implicit def `to-Int`(octet: Octet): Int = octet.octet

  /**
   * @example Given Octet(47) this will return `2f`
   * @param octet The instance to extract a value from
   * @return A string representation of the octet as two hex character without any prefix
   */
  @deprecated(since = "2.1.0")
  implicit def `to-String`(octet: Octet): String = octet.octet.formatted("%02x")

  /**
   * Implicit conversion to the unconstrained version of octet.
   * @param octet The instance to convert
   * @return An unconstrained instance of octet
   */
  implicit def `to-Octet`(octet: Octet): unconstrained.Octet = unconstrained.Octet(Some(octet.octet))

  private val HexPat = "^([0-9a-f]{1,2})$".r

  private val inRange = Range(0, 255 + 1).contains _

  /**
   * @param candidateValue A number
   * @return A some of Octet if the number is in the range [0,255] otherwise `None`
   */
  def opt(candidateValue: Int): Option[Octet] =
    Some(candidateValue) filter inRange map apply

  /**
   * @param candidateValue A number represented as hex
   * @return A some of Octet if the string is one or two hex characters otherwise `None`
   */
  def opt(candidateValue: String): Option[Octet] =
    Option(candidateValue).map(_.toLowerCase).flatMap {
      case HexPat(digits) => digits.optHexInt map apply
      case _ => None
    }

  def unapply(x: Int): Option[Int] =
    Some(x) filter inRange

  def unapply(x: String): Option[Int] =
    opt(x) map (_.octet)

  private[binary] val Zero = Octet(0)

}