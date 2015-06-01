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

import com.scalacraft.domain.v2.internal.{Information, RejectNullConstructorArgument}

import com.scalacraft.domain.v2.internal.NumericConversions.FromString

import com.scalacraft.domain.v2.binary.{OctetPair => Constrained}
import com.scalacraft.domain.v2.binary.{Octet => ConstrainedOctet}

/**
 * An unconstrained `OctetPair` represents two [[Octet]]s which are themselves unconstrained.
 *
 * In addition to using unconstrained octets each octet is optional. This allows an invalid pair to
 * be created when we have only valid octets to select from.
 *
 * When interpreted as recording a single integer value using this expression 256 * hi + lo the allowable
 * range has a minimum of -0x8080000000 and maximum equalling 0x807ffffeff.
 *
 * In decimal the representable range is [-551903297536, 551903297279].
 *
 * === Pattern Matching ===
 *
 * Pattern matching is supported as the following examples demonstrate,
 * {{{
 *   0x3490 match {
 *     case OctetPair(hi, lo) => (hi, lo) // (Some(Octet(Some(0x34))), Some(Octet(Some(0x90)))
 *     case _ => None
 *   }
 * }}}
 *
 * Matching will succeed against integer values larger than the range of two constrained octets,
 * {{{
 *   0x4251d match {
 *     case OctetPair(hi, lo) => (hi, lo) // (Some(Octet(Some(0x425))), Some(Octet(Some(0x1d)))
 *     case _ => None
 *   }
 * }}}
 *
 * Negative values are matched,
 * {{{
 *   -257 match {
 *     case OctetPair(hi, lo) => (hi, lo) // (Some(Octet(Some(-1))), Some(Octet(Some(-1)))
 *     case _ => None
 *   }
 * }}}
 *
 * The match target can be a string,
 * {{{
 * val s: String = "4020"
 *
 * s match {
 *   case OctetPair(hi, lo) => (hi, lo) // (Some(Octet(Some(0x40))), Some(Octet(Some(0x20)))
 *   case _ => None
 * }
 * }}}
 *
 * === Implicit Conversions ===
 *
 * An implicit conversion is supplied which allows an instance of `OctetPair` to be used where `Option[String]` is
 * required.
 *
 * {{{
 *  val octetPair = OctetPair(Some(Octet(0xab)), Some(Octet(0x17)))
 *  val s: Option[String] = octetPair
 *  s foreach(println) // ab17
 * }}}
 *
 * A conversion to an option of the constrained version of this class is also available.
 *
 * @param hi non-null octet option representing the high order octet in this pair
 * @param lo non-null octet option representing the low order octet in this pair
 */
case class OctetPair(hi: Option[Octet], lo: Option[Octet]) {
  RejectNullConstructorArgument(hi, "hi")
  RejectNullConstructorArgument(lo, "lo")
}

/**
 * ==Value distribution between hi and lo octets==
 *
 * With a `hi` and `lo` octet the range of representable values is bounded below by
 * {{{
 * 256 * min-int-value + min-int-value
 * }}}
 *
 * and above by
 * {{{
 * 256 * max-int-value + max-int-value
 * }}}
 *
 * When matching against a value that is inside this inclusive range,
 * {{{
 * [256 * min-int-value - 255, 256 * max-int-value + 255]
 * }}}
 *
 * the value of the `lo` octet will always be in this inclusive range,
 * {{{
 * [0, 255]
 * }}}
 *
 * For values that lie outside of this range the `lo` octet will take on values from one of
 * these two inclusive ranges,
 *
 * {{{
 * [min-int-value, -256]
 * [256, max-int-value]
 * }}}
 *
 * while the `hi` octet will be one of these two values,
 *
 * {{{
 * min-int-value
 * max-int-value
 * }}}
 *
 * ==TODO: Add match examples for value distribution==
 */
object OctetPair {

  //  implicit def `to-Int`(octetPair: OctetPair): Int = octetPair.hi * 256 + octetPair.lo

  /**
   * @example Given OctetPair(Some(Octet(Some(47))), Some(Octet(Some(128)))) this will return `2f80`
   * @param octetPair The instance to extract a value from
   * @return A string representation of the octet pair as four or more hex characters without any prefix
   */
  implicit def `to-String`(octetPair: OctetPair): Option[String] =
    for {
      hiOctet <- octetPair.hi
      hiInt <- hiOctet.octet
      loOctet <- octetPair.lo
      loInt <- loOctet.octet
    } yield {
      (BigInt(hiInt) * HiOctetMultiplier + BigInt(loInt)).formatted("%04x")
    }

  //  /**
  //   * Unwrap one level. Do not unwrap the pair of `Option[Octet]` to Option[BigInt] because this will
  //   * have little use.
  //   * @param octetPair The octet pair to unwrap
  //   * @return A pair of [[Octet]] options
  //   */
  //  implicit def `to-Octets`(octetPair: OctetPair): (Option[Octet], Option[Octet]) =
  //    (octetPair.hi, octetPair.lo)

  implicit def `to-Option[OctetPair]`(octetPair: OctetPair): Option[Constrained] = {
    for {
      hiOctet <- octetPair.hi
      consHiOctet <- Octet.`to-Option[Octet]`(hiOctet)
      loOctet <- octetPair.lo
      consLoOctet <- Octet.`to-Option[Octet]`(loOctet)
      consOctetPair <- Constrained.opt(consHiOctet, consLoOctet)
    } yield consOctetPair
  }

  /**
   * @param hi A octet. Can be null.
   * @param lo A octet. Can be null.
   * @return A new instance using `None` for the octet value when null was supplied.
   */
  def apply(hi: Octet, lo: Octet) = new OctetPair(Option(hi), Option(lo))

  // TODO: Add apply(hi: Int, lo: Int)

  def unapply(x: Int): Option[(Option[Octet], Option[Octet])] = {
    val hi = Octet(x / 256)
    val lo = Octet(x % 256)
    Some((Some(hi), Some(lo)))
  }

  private val HiOctetMultiplier = BigInt(0x100)

  private object Max {
    val FourOctets = BigInt(Int.MaxValue)
    val FiveOctets = FourOctets * HiOctetMultiplier + FourOctets
  }

  private object Min {
    val FourOctets = BigInt(Int.MinValue)
    val FiveOctets = FourOctets * HiOctetMultiplier + FourOctets
  }

  private val range = Min.FiveOctets to Max.FiveOctets

  /**
   * The numeric value of an octet is `hi*256 + lo`. Following a principle that requires the
   * unconstrained types to use as much information as possible without losing any information
   * the maximum and minimum values are,
   *
   * {{{
   * Max: max-int-value * 256 + max-int-value
   * Min: min-int-value * 256 + min-int-value
   * }}}
   * @param x
   * @return
   */
  def unapply(x: String): Option[(Option[Octet], Option[Octet])] = {

    x match {
      case Information.Zero() => None
      case n =>
        n.optHexBigInt match {
          case Some(i) if Min.FiveOctets <= i && i <= Max.FiveOctets =>

            val hi = (i / 0x100)
            val lo = (i - hi * 0x100)

            /**
             * At this point we are range checked so it is safe to transfer the
             * overflow or underway from the high octet to the low octet
             */
            val overflow = (hi - Max.FourOctets) max 0
            val (hi2, lo2) = (hi - overflow, lo + overflow * 0x100)

            val underflow = (Min.FourOctets - hi2) max 0
            val (hi3, lo3) = (hi2 + underflow, lo2 - underflow * 0x100)

            Some(Some(Octet(Some(hi3.toInt))), Some(Octet(Some(lo3.toInt))))
          case _ => None
        }
    }
  }
}