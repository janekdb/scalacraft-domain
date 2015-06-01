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

import com.scalacraft.domain.v2.internal.Information

/**
 * An `OctetPair` represents two [[Octet]]s.
 *
 * The following constraints hold for instances of this class,
 *
 * - `hi` is not null
 * - `lo` is not null
 *
 * An instance can be created using a suitable overload of the `opt` method.
 *
 * {{{
 *   val op1: Option[OctetPair] = OctetPair.opt(34540)  // OctetPair(134, 236) = 34540
 *   val op2: Option[OctetPair] = OctetPair.opt(0x86ec) // OctetPair(134, 236) = 34540
 *   val op3: Option[OctetPair] = OctetPair.opt("d") // OctetPair(0, 13)
 *   val op3: Option[OctetPair] = OctetPair.opt("fe") // OctetPair(0, 254)
 *   val op5: Option[OctetPair] = OctetPair.opt("012") // Decimal 18
 *   val op4: Option[OctetPair] = OctetPair.opt("f11d") // Decimal 61725
 * }}}
 *
 * Note that the string variant takes between one to four hex characters.
 *
 * When any class constraint is violated the result is `None`.
 * {{{
 *   val op1: Option[OctetPair] = OctetPair.opt("NaN") // None
 *   val op2: Option[OctetPair] = OctetPair.opt(0xf1234) // None
 * }}}
 *
 * === Pattern Matching ===
 *
 * Pattern matching is supported as the following examples demonstrate,
 * {{{
 *   0x3490 match {
 *     case OctetPair(hi, lo) => (hi, lo) // (Octet(0x34), Octet(0x90)
 *     case _ => None
 *   }
 * }}}
 *
 * The match target can be a string,
 * {{{
 * val s: String = "4020"
 *
 * s match {
 *   case OctetPair(hi, lo) => (hi, lo) // (Octet(0x40), Octet(0x20)
 *   case _ => None
 * }
 * }}}
 *
 * Invalid octet pairs are not matched,
 * {{{
 *   -129 match {
 *     case OctetPair(hi, lo) => (hi, lo)
 *     case _ => None // None
 *   }
 * }}}
 *
 * === Implicit Conversions ===
 *
 * Implicit conversions are supplied which allow an instance of `OctetPair` to be used when an `Int` or `String` is
 * required.
 *
 * {{{
 *   val Some(pair) = OctetPair.opt(0x40cc)
 *   val w: Int = 1 + pair // 0x40cd
 * }}}
 *
 * {{{
 *   val Some(pair) = OctetPair.opt("f")
 *   val s: String = pair: String // 000f
 * }}}
 *
 * A conversion to the unconstrained version of this class is also available.
 *
 * @param hi A valid octet representing the high byte
 * @param lo A valid octet representing the low byte
 */
case class OctetPair private(hi: Octet, lo: Octet)

object OctetPair {

  implicit def `to-Int`(octetPair: OctetPair): Int = octetPair.hi * 256 + octetPair.lo

  /**
   * @example Given OctetPair(Octet(47), Octet(128)) this will return `2f80`
   * @param octetPair The instance to extract a value from
   * @return A string representation of the octet pair as four hex character without any prefix
   */
  implicit def `to-String`(octetPair: OctetPair): String =
    Octet.`to-String`(octetPair.hi) + Octet.`to-String`(octetPair.lo)

  /**
   * Implicit conversion to the unconstrained version of octet pair.
   * @param octetPair The instance to convert
   * @return An unconstrained instance of octet pair
   */
  implicit def `to-OctetPair`(octetPair: OctetPair): unconstrained.OctetPair =
    unconstrained.OctetPair(octetPair.hi, octetPair.lo)

  // TODO: Move this to Octet.Zero
  private val ZeroOctet = Octet.opt(0).get

  def opt(x: Int): Option[OctetPair] = unapply(x) map { case (hi, lo) => OctetPair(hi, lo)}

  def opt(x: String): Option[OctetPair] = unapply(x) map { case (hi, lo) => OctetPair(hi, lo)}

  // TODO: Support this null checking pattern in Information object
  def opt(a: Octet, b: Octet): Option[OctetPair] =
    for {
      _ <- Option(a)
      _ <- Option(b)
    } yield OctetPair(a, b)

  def unapply(x: String): Option[(Octet, Octet)] = {
    val octets = extractOctets(x, Nil)
    octets match {
      case Some(lo :: Nil) => Some(ZeroOctet, lo)
      case Some(hi :: lo :: Nil) => Some(hi, lo)
      case _ => None
    }
  }

  // TODO: How to extract without knowing the range of the octet?
  def unapply(x: Int): Option[(Octet, Octet)] =
    for {
      hi <- Octet.opt(x / 256)
      lo <- Octet.opt(x % 256)
    } yield (hi, lo)

  /**
   * No assumptions are made about the format acceptable to [[Octet]]
   */
  private def extractOctets(x: String, accum: List[Octet]): Option[List[Octet]] = {
    Information.whenSome(x, accum) {
      longestRightMatch(_) match {
        case Some((unused, octet)) => extractOctets(unused, octet :: accum)
        case None => None
      }
    }
  }

  private def longestRightMatch(x: String): Option[(String, Octet)] = {
    /* For example: (,250d), (2,50d), (25,0d), (250,d) */
    val spans: Seq[(String, String)] = (0 until x.size).toList.map(x splitAt _)
    val candidates: Seq[(String, Option[Octet])] = spans.map {
      case (unused, target) => (unused, Octet.opt(target))
    }
    candidates.collectFirst { case (unused, Some(octet)) => (unused, octet)}
  }
}
