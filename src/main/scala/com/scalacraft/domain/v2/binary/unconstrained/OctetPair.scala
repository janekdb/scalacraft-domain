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

import scala.util.control.Exception._

/**
 * TODO: Documentation
 * `OctetPair`
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
 * ==TODO: Add match examples==
 */
object OctetPair {

  /**
   * @param hi A octet. Can be null.
   * @param lo A octet. Can be null.
   * @return A new instance using `None` when null was supplied.
   */
  def apply(hi: Octet, lo: Octet) = new OctetPair(Option(hi), Option(lo))

  def unapply(x: Int): Option[(Option[Octet], Option[Octet])] = {
    val hi = Octet(x / 256)
    val lo = Octet(x % 256)
    Some((Some(hi), Some(lo)))
  }

  //  private val ZeroOctet = Octet(0)

  //  import com.scalacraft.domain.v2.internal.NumericConversions.FromString

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

    // TODO: Move to NumericConversions.FromString
    def optBigInt(n: String): Option[BigInt] =
      catching(classOf[NumberFormatException]) opt BigInt(n, 0x10)

    x match {
      case Information.Zero() => None
      case n =>
        optBigInt(n) match {
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