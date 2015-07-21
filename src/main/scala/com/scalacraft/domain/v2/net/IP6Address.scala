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

// http://en.wikipedia.org/wiki/IPv6_address
/*
 In an attempt to simplify IPv6 addresses, the standards provides flexibility in their representation. However, this also complicates several common operations: searching for a specific address in a text file or stream, and comparing two addresses to determine their equivalence. To mitigate these problems, the IETF has proposed a standard in RFC 5952 for a canonical format for rendering IPv6 addresses in text. Its specific recommendations are:

 Leading zeros in each 16-bit field are suppressed. For example, 2001:0db8::0001 is rendered as 2001:db8::1, though any all-zero field that is explicitly presented is rendered as 0.
 "::" is not used to shorten just a single 0 field. For example, 2001:db8:0:0:0:0:2:1 is shortened to 2001:db8::2:1, but 2001:db8:0000:1:1:1:1:1 is rendered as 2001:db8:0:1:1:1:1:1.
 Representations are shortened as much as possible. The longest sequence of consecutive all-zero fields is replaced by double-colon. If there are multiple longest runs of all-zero fields, then it is the leftmost that is compressed. E.g., 2001:db8:0:0:1:0:0:1 is rendered as 2001:db8::1:0:0:1 rather than as 2001:db8:0:0:1::1.
 Hexadecimal digits are expressed as lower-case letters. For example, 2001:db8::1 is preferred over 2001:DB8::1.
*/

// http://en.wikipedia.org/wiki/IPv6_address
// Convert to documentation
// One or more consecutive groups of zero value may be replaced with a single empty group using two consecutive colons (::)
/**
 * `IP6Address`
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

  // TODO: Select a better name suitable for addition to all domain classes
  def toStringForm: String = {
    // TODO: Delegate to OctetPair.toStringForm
    val s = OctetPair.`to-String` _
    val fs = field1 :: field2 :: field3 :: field4 :: field5 :: field6 :: field7 :: field8 :: Nil
    fs.map(s) mkString IP6Address.GroupSeparator
  }

}

object IP6Address {

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

  def opt(x: String): Option[IP6Address] = {
    val allTokens: Option[List[Token]] = x match {
      case Information.Zero() => None
      case _ => Some(parseTokens(x.toLowerCase, Nil))
    }
    println("x: '" + x + "'")
    println(s"allTokens: $allTokens")
    val tokens = for {
      ts <- allTokens
      digitsCount = countDigitGroups(ts)
      zeros = makeZeroes(digitsCount)
      abbreviationCount = countAbbreviations(ts)
      /* Do not allow an abbreviation to be used unnecessarily */
      if abbreviationCount == 0 || digitsCount < RequiredGroupCount
      vs = ts match {
        case AB :: Nil => zeros.init
        case AB :: rest => zeros ++ rest
        case other => other
      }
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

    println(s"tokens: $tokens")

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

  private val Some(zero) = OctetPair.opt(0)

  private val RequiredGroupCount = 8

  private def countDigitGroups(tokens: Seq[Token]) =
    tokens.count {
      case D(_) => true
      case _ => false
    }

  private def countAbbreviations(tokens: Seq[Token]) = tokens.count(_ == AB)

  private def makeZeroes(count: Int): List[Token] =
    ((1 to (RequiredGroupCount - count)) flatMap { case _ => D("0") :: S :: Nil}).toList

  private def parseTokens(x: String, acc: List[Token]): List[Token] =
    nextToken(x) match {
      case Some((token, rest)) => parseTokens(rest, token :: acc)
      case None => acc
    }

  private val ColonColon = "::(.*)".r
  private val Colon = ":(.*)".r
  private val Digits = "([0-9a-f]+)(.*)".r

  private def nextToken(x: String): Option[(Token, String)] = {
    x match {
      case ColonColon(rest) => Some((AB, rest))
      case Colon(rest) => Some((S, rest))
      case Digits(digits, rest) => Some((D(digits), rest))
      case _ => None
    }
  }

  sealed trait Token

  /* The colon separator between octet pairs */
  case object S extends Token

  /* The abbreviation used to represent a group of zero or more zeroes */
  case object AB extends Token

  case class D(digits: String) extends Token

  private val GroupSeparator = ":"
}