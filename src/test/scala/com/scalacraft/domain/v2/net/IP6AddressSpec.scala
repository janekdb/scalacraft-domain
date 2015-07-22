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
    val InternalZeroAbbreviation = "ff::1"
    val LeftZeroAbbreviation = "::1"
    val RightZeroAbbreviation = "ff::"
    val StandaloneAbbreviation = "::"
    val OneZeroGroupAbbreviatedRight = "0:1:2:3:4:5:6::"
    val OneZeroGroupAbbreviatedLeft = "::1:2:3:4:5:6:7"
    val OneZeroGroupAbbreviatedInternal = "0:1:2::4:5:6:7"

    //    val ValidQuad = "240.1.255.7"
    //    val InvalidQuad = "240.1.255.B"
    //    val RangeExceededByte = "240.256.234.7"
    //    val TrailingDot = ValidQuad + "."
    //    val NonNumeric = "a.b.c.d"
  }

  private object InvalidStrings {
    val FiveDigits = "0:0:0:0:0:0:12345:0"
    val Negative = "0:0:0:0:0:0:-99aa:0"
    val TwoZeroGroupAbbreviations = "1::1::"
    val TrailingDot = "1:2:3:4:5:6:7:8."
    val NonNumeric = "1k:2:3:4:5:6:7:8"
    val FourGroups = "1:22:333:4444"
  }

  it should "be constructed from a valid full string representation" in {
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

  it should "be constructed from a valid zero group string representation" in {
    IP6Address.opt(ValidStrings.InternalZeroAbbreviation).value should have(
      'field1(op(0xff)),
      'field2(zero),
      'field3(zero),
      'field4(zero),
      'field5(zero),
      'field6(zero),
      'field7(zero),
      'field8(op(0x1))
    )
  }

  it should "be constructed from a left zero group string representation" in {
    IP6Address.opt(ValidStrings.LeftZeroAbbreviation).value should have(
      'field1(zero),
      'field2(zero),
      'field3(zero),
      'field4(zero),
      'field5(zero),
      'field6(zero),
      'field7(zero),
      'field8(op(0x1))
    )
  }

  it should "be constructed from a right zero group string representation" in {
    IP6Address.opt(ValidStrings.RightZeroAbbreviation).value should have(
      'field1(op(0xff)),
      'field2(zero),
      'field3(zero),
      'field4(zero),
      'field5(zero),
      'field6(zero),
      'field7(zero),
      'field8(zero)
    )
  }

  it should "be constructed from a standalone zero group string representation" in {
    IP6Address.opt(ValidStrings.StandaloneAbbreviation).value should have(
      'field1(zero),
      'field2(zero),
      'field3(zero),
      'field4(zero),
      'field5(zero),
      'field6(zero),
      'field7(zero),
      'field8(zero)
    )
    IP6Address.opt("::23af:0091::") should be(None)
  }

  it should "be constructed from an single zero group abbreviation representation" in {
    IP6Address.opt(ValidStrings.OneZeroGroupAbbreviatedRight).value should have(
      'field1(zero),
      'field2(one),
      'field3(two),
      'field4(three),
      'field5(four),
      'field6(five),
      'field7(six),
      'field8(zero)
    )
    IP6Address.opt(ValidStrings.OneZeroGroupAbbreviatedLeft).value should have(
      'field1(zero),
      'field2(one),
      'field3(two),
      'field4(three),
      'field5(four),
      'field6(five),
      'field7(six),
      'field8(seven)
    )
    IP6Address.opt(ValidStrings.OneZeroGroupAbbreviatedInternal).value should have(
      'field1(zero),
      'field2(one),
      'field3(two),
      'field4(zero),
      'field5(four),
      'field6(five),
      'field7(six),
      'field8(seven)
    )
  }

  it should "not be constructed when groups missing and multiple abbreviations" in {
    /* internal, left, right, both */
    IP6Address.opt("77:ff::ee::0") should be(None)
    IP6Address.opt("::23be::0091") should be(None)
    IP6Address.opt("24ae::0091::") should be(None)
    IP6Address.opt("::27ae:0091::") should be(None)
  }

  it should "not be constructed from an unnecessary zero group string representation" in {
    /* This already has 8 groups defined. Do not accept :: */
    val invalid1 = "ff::" + "0:" * 6 + "0"
    IP6Address.opt(invalid1) should be(None)
    /* Using :: to add one 0 group is not accepted. */
    val invalid2 = "ff::" + "0:" * 6
    IP6Address.opt(invalid2) should be(None)
  }

  it should "not be constructed from a invalid string representation" in {
    val validSuffix = ":1" * 7

    IP6Address.opt(" ") should be(None)
    IP6Address.opt(" " + ValidStrings.AllZeros) should be(None)
    IP6Address.opt("$" + validSuffix) should be(None)
    IP6Address.opt(InvalidStrings.FiveDigits) should be(None)
    IP6Address.opt(InvalidStrings.Negative) should be(None)
    IP6Address.opt(InvalidStrings.TwoZeroGroupAbbreviations) should be(None)
    IP4Address.opt(InvalidStrings.TrailingDot) should be(None)
    IP4Address.opt(InvalidStrings.NonNumeric) should be(None)
    IP4Address.opt(InvalidStrings.FourGroups) should be(None)
  }

  it should "not be constructed from a null string" in {
    IP6Address.opt(null) should be(None)
  }

  it should "not be constructed from an empty string" in {
    IP6Address.opt("") should be(None)
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
    def m(x: String) = x match {
      case IP6Address(op1, op2, op3, op4, op5, op6, op7, op8) =>
        (op1, op2, op3, op4, op5, op6, op7, op8)
      case _ => None
    }
    m(InvalidStrings.FourGroups) should be(None)
    m("0:01:02:03:04:0005:0006:0007") should be(
      (zero, one, two, three, four, five, six, seven))
    m(ValidStrings.StandaloneAbbreviation) should be(
      (zero, zero, zero, zero, zero, zero, zero, zero))
  }

  /* Explicit Conversions */

  it should "convert to a string representation" in {
    IP6Address.opt("::").value should have(
      'representation("0000:0000:0000:0000:0000:0000:0000:0000"))

    IP6Address.opt("1:23:456:789f::ffea").value should have(
      'representation("0001:0023:0456:789f:0000:0000:0000:ffea"))

    IP6Address.opt("::fedc").value should have(
      'representation("0000:0000:0000:0000:0000:0000:0000:fedc"))
  }

  it should "convert to an unconstrained IP6Address" in {
    //    val ipa: IP4Address = IP4Address.opt(88, 0, 2, 119).get
    //    val other: Other = ipa
    //    other should have(
    //      'byte1(88),
    //      'byte2(0),
    //      'byte3(2),
    //      'byte4(119)
    //    )
  }

  /* zero groups abbreviations in toStringForm */

  it should "xxxxx" in {

  }

  private def op(x: Int): OctetPair = OctetPair.opt(x).get

}
