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
package com.scalacraft.domain.v2.net

import com.scalacraft.domain.v2.binary.OctetPair
import com.scalacraft.domain.v2.internal.Information
import com.scalacraft.domain.v2.internal.IP6AddressRepresentation
import com.scalacraft.domain.v2.internal.IP6AddressRepresentation.{Token, S, AB, D}
import com.scalacraft.domain.v2.net.unconstrained.{IP6Address => Unconstrained}

/**
 * An `IP6Address` represents an IP6 address.
 *
 * The address is comprised of 8 constrained octet pairs.
 *
 * The rules concerning valid string representations for ip6 addresses are taken from http://en.wikipedia.org/wiki/IPv6_address.
 *
 * === Pattern Matching ===
 *
 * Pattern matching is supported as the following example demonstrates,
 * {{{
 * &qt;01::0006:0010&qt; match {
 *   case IP6Address(op1, op2, _, _, _, _, _, op8) =>
 *   (op1, op2, op8) // 0x0001, 0x0000, 0x0010
 *   case _ => None
 * }
 * }}}
 *
 * === Limitations ===
 *
 * The special format that allows for a trailing dotted quad for the rightmost two 16 bit fields is not supported.
 *
 * As shown in the example above the shortening of groups of zeros with a double colon is recognised.
 *
 * A conversion to the unconstrained version of this class is also available.
 */
case class IP6Address private(
                               field1: OctetPair,
                               field2: OctetPair,
                               field3: OctetPair,
                               field4: OctetPair,
                               field5: OctetPair,
                               field6: OctetPair,
                               field7: OctetPair,
                               field8: OctetPair
                               ) {

  // TODO: Add and use OctetPair.unconstrained
  /**
   * Convert to the unconstrained version of ip6 address.
   * @return An unconstrained instance of ip6 address
   */
  def unconstrained: Unconstrained = new Unconstrained(fields map OctetPair.`to-OctetPair`)

  /**
   * Provide a string representation of this ip6 address in RFC5952 recommended form.
   * - colon separator
   * - lowercase hexadecimal digits
   * - no leading zero digits
   * - leftmost longest sequence of two or more zero groups abbreviated with double colon
   * @example 0:10:20::dd01:3
   * @return A string representation using a colon separator, lowercase hexadecimal digits without leading zeroes
   */
  def representation: String = IP6Address.representation(this)

  private def fields = field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: Nil
}

object IP6Address {

  import IP6AddressRepresentation.RequiredGroupCount

  private def representation(ip6Address: IP6Address): String = {

    val s = (op: OctetPair) => OctetPair.`to-Int`(op) formatted "%x"

    IP6AddressRepresentation.representation(ip6Address.fields map s)
  }

  def opt(
           o1: OctetPair,
           o2: OctetPair,
           o3: OctetPair,
           o4: OctetPair,
           o5: OctetPair,
           o6: OctetPair,
           o7: OctetPair,
           o8: OctetPair
           ): Option[IP6Address] =
    Information.whenNotNull(o1, o2, o3, o4, o5, o6, o7, o8)(apply)

  private val tokenParser = new IP6AddressRepresentation.TokenParser {
    val Digits = "([0-9a-f]++)(.*)".r
  }

  def opt(x: String): Option[IP6Address] = {
    val allTokens: Option[List[Token]] = x match {
      case Information.Zero() => None
      case _ => tokenParser.parseTokens(x.toLowerCase, Nil)
    }
    val tokens = for {
      ts <- allTokens
      digitsCount = countDigitGroups(ts)
      abbreviationCount = countAbbreviations(ts)
      /* Do not allow an abbreviation to be used unnecessarily */
      if abbreviationCount == 0 || digitsCount < RequiredGroupCount
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
    } yield ss

    for {
      D(d1) :: S :: D(d2) :: S :: D(d3) :: S :: D(d4) :: S :: D(d5) :: S :: D(d6) :: S :: D(d7) :: S :: D(d8) :: Nil <- tokens
      op1 <- OctetPair.opt(d1)
      op2 <- OctetPair.opt(d2)
      op3 <- OctetPair.opt(d3)
      op4 <- OctetPair.opt(d4)
      op5 <- OctetPair.opt(d5)
      op6 <- OctetPair.opt(d6)
      op7 <- OctetPair.opt(d7)
      op8 <- OctetPair.opt(d8)
    }
      yield IP6Address(op1, op2, op3, op4, op5, op6, op7, op8)
  }

  def unapply(candidate: String): Option[(OctetPair, OctetPair, OctetPair, OctetPair, OctetPair, OctetPair, OctetPair, OctetPair)] =
    opt(candidate) map {
      ip6 => (
        ip6.field1,
        ip6.field2,
        ip6.field3,
        ip6.field4,
        ip6.field5,
        ip6.field6,
        ip6.field7,
        ip6.field8
        )
    }

  private def countAbbreviations(tokens: Seq[Token]) = tokens.count(_ == AB)

  private val Some(zero) = OctetPair.opt(0)

  private def countDigitGroups(tokens: Seq[Token]) =
    tokens.count {
      case D(_) => true
      case _ => false
    }

  /**
   * @param digitsCount The number of digits group present
   * @return A list of D(0), S, ... sufficiently long to ensure a list of 8 digit groups when
   *         combined with the existing digit groups
   */
  private def makeZeroes(digitsCount: Int): List[Token] =
    ((1 to (RequiredGroupCount - digitsCount)) flatMap { case _ => D("0") :: S :: Nil }).toList

}