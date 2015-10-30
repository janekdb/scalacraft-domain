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
package com.scalacraft.domain.v2.net.unconstrained

import com.scalacraft.domain.v2.binary.{OctetPair => ConstrainedOctetPair}
import com.scalacraft.domain.v2.binary.unconstrained.{Octet, OctetPair}
import com.scalacraft.domain.v2.internal.{Information, IP6AddressRepresentation, RejectNullConstructorArgument}
import com.scalacraft.domain.v2.internal.IP6AddressRepresentation.{Token, S, AB, D}
import com.scalacraft.domain.v2.net.{IP6Address => Constrained}

/**
 * An unconstrained `IP6Address` represents an IP6 address that could have missing data or unconstrained
 * data.
 *
 * The address is comprised of a list of unconstrained octet pairs.
 *
 * The rules concerning valid string representations for ip6 addresses are taken from http://en.wikipedia.org/wiki/IPv6_address.
 *
 * === Pattern Matching ===
 *
 * Pattern matching is supported as the following examples demonstrate,
 * {{{
 * &qt;01::0006:0010&qt; match {
 *   case IP6Address(octetPairs) =>
 *   (octetPairs(0), octetPairs(1), octetPairs(2)) // 0x0001, 0x0000, 0x0010
 * }
 *
 * &qt;7::8&qt; match {
 *   case IP6Address(octetPairs) => octetPairs // 7, 0, 0, 0, 0, 0, 0, 8
 * }
 * }}}
 *
 * === Limitations ===
 *
 * The special format that allows for a trailing dotted quad for the rightmost two 16 bit fields is not supported.
 *
 * As shown in the example above the shortening of groups of zeros with a double colon is recognised.
 *
 * A conversion to an option of the constrained version of this class is also available.
 *
 * @param octetPairs non-null octet pair list representing this ip6 address. The list can have any size.
 */
case class IP6Address(
                       octetPairs: List[OctetPair]
                       ) {
  RejectNullConstructorArgument(octetPairs, "octetPairs")
  RejectNullConstructorArgument.rejectNullElement(octetPairs, "octetPairs")

  // TODO: Add and use OctetPair.constrained
  /**
   * Convert to the constrained version of ip6 address.
   * @return An constrained instance of ip6 address as a some or none if this instance
   *         does not convert to a constrained instance
   */
  def constrained: Option[Constrained] = {
    val constrainedOctetPairs: List[Option[ConstrainedOctetPair]] = octetPairs map OctetPair.`to-Option[OctetPair]`
    type C = ConstrainedOctetPair
    for {
      Some(o1: C) :: Some(o2: C) :: Some(o3: C) :: Some(o4: C) :: Some(o5: C) :: Some(o6: C) :: Some(o7: C) :: Some(o8: C) :: Nil <- Some(constrainedOctetPairs)
      ip6Address <- Constrained.opt(o1, o2, o3, o4, o5, o6, o7, o8)
    }
      yield ip6Address
  }

  /**
   * Provide a string representation of this unconstrained ip6 address based on RFC5952 recommended form.
   * - colon separator
   * - lowercase hexadecimal digits
   * - minus signs where needed
   * - no leading zero digits
   * - leftmost longest sequence of two or more zero groups abbreviated with double colon when eight fields
   *
   * The zero group abbreviation is restricted to the case when there are eight fields to ensure the value is preserved when
   * the abbreviation is expanded. For example if the unconstrained value was `0:0:0:5` then an representation of `::5` would
   * expand to `0:0:0:0:0:0:0:5` which is not the same value.
   *
   * @example 0:10:20::dd01:3
   * @example 0:0:0:-1:-2:dd01:3
   * @return A string representation using a colon separator, lowercase hexadecimal digits without leading zeroes when possible
   *         otherwise None
   */
  def representation: Option[String] = IP6Address.representation(this.octetPairs)
}

object IP6Address {

  import IP6AddressRepresentation.RequiredGroupCount

  // TODO: Move this detail back into OctetPair
  private val HiOctetMultiplier = BigInt(0x100)

  private val representationReducers: Map[Int, List[String] => String] = Map(
    /* Allow zero group abbreviation when later expansion would result in the same value. */
    RequiredGroupCount -> IP6AddressRepresentation.representation _
  ) withDefaultValue IP6AddressRepresentation.representationWithoutAbbreviation _

  private def representation(octetPairs: List[OctetPair]): Option[String] = {
    for {
      ops <- Option(octetPairs)
      reps = ops.collect {
        case OctetPair(Some(Octet(Some(hi))), Some(Octet(Some(lo)))) =>
          (BigInt(hi) * HiOctetMultiplier + BigInt(lo)).formatted("%x")
      }
      if reps.size == ops.size
    } yield {
      representationReducers(reps.size)(reps)
    }
  }

  private val tokenParser = new IP6AddressRepresentation.TokenParser {
    val Digits = "(-?[0-9a-f]++)(.*)".r
  }

  def opt(x: String): Option[IP6Address] = {
    val allTokens: Option[List[Token]] = x match {
      case Information.Zero() => None
      case _ => tokenParser.parseTokens(x.toLowerCase, Nil)
    }
    opt(allTokens)
  }

  private def opt(allTokens: Option[List[Token]]): Option[IP6Address] = {
    val elems: Option[List[Any]] =
      for {
        tokens <- allTokens
        ts = tokens map {
          case D(x) => OctetPair.opt(x)
          case other => other
        }
        digitsCount = countOctetPairs(ts)
        abbreviationCount = countAbbreviations(ts)
        /* Do not allow abbreviations to be used when more than one possible expansion exists. */
        if abbreviationCount <= 1
        zeros = makeZeroes(digitsCount)
        /* Replace leading abbreviations */
        vs = ts match {
          case AB :: Nil => zeros.init // drop trailing separator
          case AB :: rest => zeros ++ rest
          case other => other
        }
        /* Replace trailing abbreviations */
        us = vs.reverse match {
          case AB :: rest => zeros ++ rest
          case other => other
        }
        /* Replace internal abbreviations */
        ss = us.flatMap {
          case AB => S :: zeros
          case t => t :: Nil
        }
      }
        yield ss
    // Need to match D [S D}*
    for {
      Alternating(octetPairs) <- elems
    }
      yield IP6Address(octetPairs)
  }

  private def countAbbreviations(tokens: Seq[Any]) = tokens.count(_ == AB)

  private val zero = OctetPair.opt(0)

  private def countOctetPairs(tokens: Seq[Any]) =
    tokens.count {
      case Some(_: OctetPair) => true
      case _ => false
    }

  /**
   * @param digitsCount The number of digits group present
   * @return A list of D(0), S, ... sufficiently long to ensure a list of 8 digit groups when
   *         combined with the existing digit groups
   */
  private def makeZeroes(digitsCount: Int): List[Any] =
    ((1 to (RequiredGroupCount - digitsCount)) flatMap { case _ => zero :: S :: Nil }).toList

  private object Alternating {
    /** @return An octet pair list iff `elems` is alternating sequence of digits and separators bookended with digits: D, S, D, S, ..., D */
    def unapply(elems: List[Any]): Option[List[OctetPair]] = collect(elems, Nil)

    private def collect(elems: List[Any], acc: List[OctetPair]): Option[List[OctetPair]] = {
      elems match {
        case Nil => Some(acc.reverse)
        case Some(op: OctetPair) :: Nil => collect(Nil, op :: acc)
        case Some(op: OctetPair) :: S :: Some(op2: OctetPair) :: rest => collect(Some(op2) :: rest, op :: acc)
        case _ => None
      }
    }
  }

  def unapply(candidate: String): Option[List[OctetPair]] = opt(candidate) map (_.octetPairs)
}