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
package com.scalacraft.domain.v2.binary.unconstrained

import com.scalacraft.domain.v2.binary.{Octet => Constrained}
import com.scalacraft.domain.v2.internal.Information
import com.scalacraft.domain.v2.internal.NumericConversions.FromString
import com.scalacraft.domain.v2.internal.RejectNullConstructorArgument

/**
 * An `Octet` represents an integer in the range [0, 255].
 *
 * This class does not constrain the value of the octet beyond requiring a non-null constructor argument.
 *
 * The value maybe outside the given range or undefined.
 *
 * === Pattern Matching ===
 *
 * Pattern matching is supported as the following examples demonstrate,
 * {{{
 *   5 match {
 *     case Octet(n) => n // Some(5)
 *     case _ => None
 *   }
 * }}}
 *
 * The match target can be a string,
 * {{{
 *   val s: String = "ff"
 *   s match {
 *     case Octet(n) => n // Some(255)
 *     case _ => None
 *   }
 * }}}
 *
 * A null or whitespace string will match to Octet(None) while a non-hex value will not match.
 * {{{
 *   val s: String = " " * 4
 *   s match {
 *     case Octet(n) => n  // None
 *     case _ => None
 *   }
 * }}}
 * {{{
 *   val s = "foobarbaz"
 *   s match {
 *     case Octet(n) => n
 *     case _ => None  // None
 *   }
 * }}}
 * === Implicit Conversions ===
 *
 * Implicit conversions exists which allow an instance of `Octet` to be used when an `Option[Int]` or
 * `Option[String]` is required.
 *
 * {{{
 *   val octet = Octet(1022)  // The value is not constrained
 *   val i: Option[Int] = octet  // Some(1022)
 * }}}
 *
 * A conversion to an option of the constrained version of this class is also available.
 *
 * @param octet An optional octet value
 */
case class Octet(octet: Option[Int]) {
  RejectNullConstructorArgument(octet, "octet")

  /**
   * Convert to the constrained version of octet.
   * @return An constrained instance of octet as a some or none if this instance
   *         does not convert to a constrained instance
   */
  def constrained: Option[Constrained] = octet flatMap Constrained.opt

}

object Octet {

  @deprecated(since = "2.1.0")
  implicit def `to-Option-Int`(octet: Octet): Option[Int] = octet.octet

  /**
   * @example Given Octet(47) this will return `2f`
   * @param octet The instance to extract a value from
   * @return A string representation of the octet as at least two hex characters with no `0x` prefix.
   * @example `f00f` or `0b`
   */
  @deprecated(since = "2.1.0")
  implicit def `to-Option-String`(octet: Octet): Option[String] = octet.octet.map(_.formatted("%02x"))

  @deprecated(since = "2.1.0")
  implicit def `to-Option[Octet]`(octet: Octet): Option[Constrained] = octet.constrained

  /**
   * @param octet A defined octet value
   * @return A new instance
   */
  def apply(octet: Int) = new Octet(Some(octet))

  def unapply(x: Int): Option[Option[Int]] = Some(Some(x))

  def unapply(x: String): Option[Option[Int]] = Information.whenSome(x)(_.optHexInt)
}