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
import com.scalacraft.domain.v2.internal.Reflections
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.OptionValues._

/**
 * specification for `IP6Address`
 */
class IP6AddressSpec extends FlatSpec with Matchers {


  //  private val ValidOctetPairs =
  //    "0001" :: "0002" :: "0003" :: "0004" :: "0005" :: "0006" :: "0007" :: "0008" ::
  //      Nil map (OctetPair.opt(_).get)

  //  private val ValidDottedQuad = (192, 168, 0, 1)

  //  private val FormattedValidDottedQuad = "192.168.0.1"

  //  private val InvalidByte = 256

  private object Octets {
    val Some(zero) = OctetPair.opt(0)
    val Some(one) = OctetPair.opt(1)
    val Some(two) = OctetPair.opt(2)
    val Some(three) = OctetPair.opt(3)
    val Some(four) = OctetPair.opt(4)
    val Some(five) = OctetPair.opt(5)
    val Some(six) = OctetPair.opt(6)
    val Some(seven) = OctetPair.opt(7)
  }

  import Octets._

  behavior of "A constrained IP6Address"

  /* Construction from octet pairs */

  it should "be constructed from valid inputs" in {
    val ip6Opt: Option[IP6Address] = IP6Address.opt(
      zero,
      one,
      two,
      three,
      four,
      five,
      six,
      seven
    )
    ip6Opt.value should have(
      'field1(zero),
      'field2(one),
      'field3(two),
      'field4(three),
      'field5(four),
      'field6(five),
      'field7(six),
      'field8(seven)
    )
  }

  it should "not be constructed from invalid input" in {
    val ip6Opt: Option[IP6Address] = IP6Address.opt(
      zero,
      one,
      two,
      three,
      null,
      five,
      six,
      seven
    )
    ip6Opt should be(None)
  }

  /* Construction from a string */

  private object ValidStrings {
    val AllZeros = "0000:" * 7 + "0000"
    val Ascending = "0102:0304:0506:0708:090A:0B0C:0D0E:0F10"
    val Lowercase = Ascending.toLowerCase
    val Uppercase = Ascending.toUpperCase
    //    val ValidQuad = "240.1.255.7"
    //    val InvalidQuad = "240.1.255.B"
    //    val RangeExceededByte = "240.256.234.7"
    //    val TrailingDot = ValidQuad + "."
    //    val NonNumeric = "a.b.c.d"
  }

  private object InvalidStrings {
    val FiveDigits = "0:0:0:0:0:0:12345:0"
  }

  it should "be constructed from a valid string representation" in {
    IP6Address.opt(ValidStrings.AllZeros).value should have(
      'field1(zero),
      'field2(zero),
      'field3(zero),
      'field4(zero),
      'field5(zero),
      'field6(zero),
      'field7(zero),
      'field8(zero)
    )
    IP6Address.opt(ValidStrings.Lowercase).value should have(
      'field1(op(0x0102)),
      'field2(op(0x0304)),
      'field3(op(0x0506)),
      'field4(op(0x0708)),
      'field5(op(0x090A)),
      'field6(op(0x0B0C)),
      'field7(op(0x0D0E)),
      'field8(op(0x0F10))
    )
    IP6Address.opt(ValidStrings.Uppercase).value should have(
      'field1(op(0x0102)),
      'field2(op(0x0304)),
      'field3(op(0x0506)),
      'field4(op(0x0708)),
      'field5(op(0x090A)),
      'field6(op(0x0B0C)),
      'field7(op(0x0D0E)),
      'field8(op(0x0F10))
    )
    IP6Address.opt(ValidStrings.Ascending).value should have(
      'field1(op(0x0102)),
      'field2(op(0x0304)),
      'field3(op(0x0506)),
      'field4(op(0x0708)),
      'field5(op(0x090A)),
      'field6(op(0x0B0C)),
      'field7(op(0x0D0E)),
      'field8(op(0x0F10))
    )
  }

  private def op(x: Int): OctetPair = {
    OctetPair.opt(x).get
  }

  it should "not be constructed from a invalid string representation" in {
    val validSuffix = ":1" * 7

    IP6Address.opt(null) should be(None)
    IP6Address.opt("") should be(None)
    IP6Address.opt(" ") should be(None)
    IP6Address.opt(" " + ValidStrings.AllZeros) should be(None)
    IP6Address.opt("$" + validSuffix) should be(None)
    IP6Address.opt(InvalidStrings.FiveDigits) should be(None)
    //    IP4Address.opt(Strings.RangeExceededByte) should be(None)
    //    IP4Address.opt(Strings.TrailingDot) should be(None)
    //    IP4Address.opt(Strings.NonNumeric) should be(None)
  }

  it should "not be constructed from a null string" in {
    //    IP4Address.opt(null: String) should be(None)
  }

  it should "not be constructed from an empty string" in {
    //    IP4Address.opt("") should be(None)
  }

  /* Constructor access */

  it should "not have a public constructor" in {
    val constructors = Reflections.declaredConstructors[IP6Address]
    constructors should have size 1
    val con = constructors.head
    con shouldBe 'private
  }

  it should "not allow direct instantiation" in {
    "new IP6Address(zero, zero, zero, zero, zero, zero, zero, zero)" shouldNot compile
  }

  /* Pattern Matching */

  it should "be usable in string pattern matching" in {
    //    def m(x: String) = x match {
    //      case IP4Address(b1, b2, b3, b4) => Some(b1, b2, b3, b4)
    //      case _ => None
    //    }
    //    m(Strings.InvalidQuad) should be(None)
    //    m(Strings.NonNumeric) should be(None)
    //    m(Strings.ValidQuad).value should equal((240, 1, 255, 7))
  }

  /* Implicit Conversions */

  it should "implicitly convert to a string" in {
    //    val ip4Opt: Option[IP4Address] = IP4Address.opt(
    //      ValidDottedQuad._1,
    //      ValidDottedQuad._2,
    //      ValidDottedQuad._3,
    //      ValidDottedQuad._4
    //    )
    //    val s: String = ip4Opt.get
    //    /* n.n.n.n */
    //    s should equal(FormattedValidDottedQuad)
  }

  it should "implicitly convert to an unconstrained IP4Address" in {
    //    val ipa: IP4Address = IP4Address.opt(88, 0, 2, 119).get
    //    val other: Other = ipa
    //    other should have(
    //      'byte1(88),
    //      'byte2(0),
    //      'byte3(2),
    //      'byte4(119)
    //    )
  }

  /* Representation shortening */

  it should "xxxxx" in {

  }
}
