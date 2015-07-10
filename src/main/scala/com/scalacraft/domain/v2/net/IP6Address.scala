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
                               )

object IP6Address {

  private val Some(zero) = OctetPair.opt(0)

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
    val tokens: Option[List[Token]] = x match {
      case Information.Zero() => None
      case _ => Some(parseTokens(x.toLowerCase, Nil))
    }
    // TODO: tokenise :: as a new case SS and then replace all SS occurrences with the missing
    // zeros. This will prevent use of more than one :: because the list will be too long in
    // that case.
    for {
      D(d1) :: S :: D(d2) :: S :: D(d3) :: S :: D(d4) :: S :: D(d5) :: S :: D(d6) :: S :: D(d7) :: S :: D(d8) :: Nil <- tokens
      op8 <- OctetPair.opt(d1)
      op7 <- OctetPair.opt(d2)
      op6 <- OctetPair.opt(d3)
      op5 <- OctetPair.opt(d4)
      op4 <- OctetPair.opt(d5)
      op3 <- OctetPair.opt(d6)
      op2 <- OctetPair.opt(d7)
      op1 <- OctetPair.opt(d8)
    }
    yield IP6Address(op1, op2, op3, op4, op5, op6, op7, op8)
  }

  private def parseTokens(x: String, acc: List[Token]): List[Token] = {
    nextToken(x) match {
      case Some((token, rest)) => parseTokens(rest, token :: acc)
      case None => acc
    }
  }

  //  private val ColonColon = "::([^:]*)".r
  private val Colon = ":(.*)".r
  private val Digits = "([0-9a-f]+)(.*)".r

  private def nextToken(x: String): Option[(Token, String)] = {
    x match {
      case Colon(rest) => Some((S, rest))
      case Digits(digits, rest) => Some((D(digits), rest))
      case _ => None
    }
  }

  sealed trait Token

  // :
  case object S extends Token

  // ::
  case object SS extends Token

  // 1-4 digits
  case class D(digits: String) extends Token

  //  unapply(x) map { case (hi, lo) => OctetPair(hi, lo)}

}