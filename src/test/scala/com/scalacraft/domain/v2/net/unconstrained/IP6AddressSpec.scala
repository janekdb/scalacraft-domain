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

//import com.scalacraft.domain.v2.binary.{Octet => ConstrainedOctet}

import com.scalacraft.domain.v2.binary.{OctetPair => ConstrainedOctetPair}
import com.scalacraft.domain.v2.binary.unconstrained.{Octet, OctetPair}
import com.scalacraft.domain.v2.internal.ex.NullConstructorArgumentException
import com.scalacraft.domain.v2.internal.ex.NullElementException

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.OptionValues._

/**
 * Specification for an unconstrained `IP6Address`
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
    val Some(eight) = OctetPair.opt(8)
  }

  import Octets._

  behavior of "An unconstrained IP6Address"

  /* Constructor args */

  it should "reject null constructor args" in {

    the[NullConstructorArgumentException] thrownBy {
      new IP6Address(null)
    } should have('paramName("octetPairs"))
  }

  it should "reject constructor args that embed nulls" in {
    val op1 = new OctetPair(None, None)
    val op2: OctetPair = null
    val invalidOctetPairs = op1 :: op2 :: Nil

    the[NullElementException] thrownBy {
      new IP6Address(invalidOctetPairs)
    } should have(
      'paramName("octetPairs"),
      'index(1),
      'message("octetPairs(1)")
    )
  }

  it should "be constructed from valid inputs" in {
    val ip6 = IP6Address(zero :: one :: two :: Nil)
    // TODO: Use representation when available
    ip6.octetPairs.map(OctetPair.`to-String`).map(_.get) mkString ":" should be("0000:0001:0002")
  }

  /* Construction from a string */

  private object ValidStrings {
    val AllZeros = "0000:" * 7 + "0000"
    val Ascending = "0102:0304:0506:0708:090A:0B0C:0D0E:0F10"
    val Lowercase = Ascending.toLowerCase
    val Uppercase = Ascending.toUpperCase
    val Negative = "0:0:0:0:0:0:-99aa:0"
    val InternalZeroAbbreviation = "ff::1"
    val LeftZeroAbbreviation = "::1"
    val RightZeroAbbreviation = "ff::"
    val StandaloneAbbreviation = "::"
    val OneZeroGroupAbbreviatedRight = "0:1:2:3:4:5:6::"
    val OneZeroGroupAbbreviatedLeft = "::1:2:3:4:5:6:7"
    val OneZeroGroupAbbreviatedInternal = "0:1:2::4:5:6:7"
  }

  it should "be constructed from a valid full string representation" in {
    IP6Address.opt(ValidStrings.AllZeros).value should have(
      'octetPairs(List.fill(8)(zero))
    )

    val expected = op(0x0102) :: op(0x0304) :: op(0x0506) :: op(0x0708) :: op(0x090A) :: op(0x0B0C) :: op(0x0D0E) :: op(0x0F10) :: Nil

    IP6Address.opt(ValidStrings.Lowercase).value should have(
      'octetPairs(expected)
    )
    IP6Address.opt(ValidStrings.Uppercase).value should have(
      'octetPairs(expected)
    )
    IP6Address.opt(ValidStrings.Ascending).value should have(
      'octetPairs(expected)
    )
  }

  it should "be constructed from a valid full string representation including a negative entry" in {
    val expected = zero :: zero :: zero :: zero :: zero :: zero :: op(-0x99aa) :: zero :: Nil
    IP6Address.opt(ValidStrings.Negative).value should have(
      'octetPairs(expected)
    )
  }

  it should "be constructed from a valid zero group string representation" in {
    IP6Address.opt(ValidStrings.InternalZeroAbbreviation).value should have(
      'octetPairs(op(0x00ff) :: zero :: zero :: zero :: zero :: zero :: zero :: op(0x0001) :: Nil)
    )
  }

  it should "be constructed from a left zero group string representation" in {
    IP6Address.opt(ValidStrings.LeftZeroAbbreviation).value should have(
      'octetPairs(zero :: zero :: zero :: zero :: zero :: zero :: zero :: op(0x0001) :: Nil)
    )
  }

  it should "be constructed from a right zero group string representation" in {
    IP6Address.opt(ValidStrings.RightZeroAbbreviation).value should have(
      'octetPairs(op(0x00ff) :: zero :: zero :: zero :: zero :: zero :: zero :: zero :: Nil)
    )
  }

  it should "be constructed from a standalone zero group string representation" in {
    IP6Address.opt(ValidStrings.StandaloneAbbreviation).value should have(
      'octetPairs(List.fill(8)(zero))
    )
    IP6Address.opt("::23af:0091::") should be(None)
  }

  it should "be constructed from an single zero group abbreviation representation" in {
    IP6Address.opt(ValidStrings.OneZeroGroupAbbreviatedRight).value should have(
      'octetPairs(zero :: one :: two :: three :: four :: five :: six :: zero :: Nil)

    )
    IP6Address.opt(ValidStrings.OneZeroGroupAbbreviatedLeft).value should have(
      'octetPairs(zero :: one :: two :: three :: four :: five :: six :: seven :: Nil)
    )
    IP6Address.opt(ValidStrings.OneZeroGroupAbbreviatedInternal).value should have(
      'octetPairs(zero :: one :: two :: zero :: four :: five :: six :: seven :: Nil)
    )
  }

  private object ValidShortStrings {
    val AllZeros = "0000:" * 3 + "0000"
    val Ascending = "0102:0304:0506"
  }

  it should "be constructed from a valid short string representation" in {
    IP6Address.opt(ValidShortStrings.AllZeros).value should have(
      'octetPairs(List.fill(4)(zero))
    )

    val expected = op(0x0102) :: op(0x0304) :: op(0x0506) :: Nil

    IP6Address.opt(ValidShortStrings.Ascending).value should have(
      'octetPairs(expected)
    )
  }

  private object ValidLongStrings {
    val AllZeros = "0000:" * 17 + "0000"
    val Ascending = "0102:0304:0506:0708:090A:0B0C:0D0E:0F10:9080:9070:9060"
  }

  it should "be constructed from a valid long string representation" in {
    IP6Address.opt(ValidLongStrings.AllZeros).value should have(
      'octetPairs(List.fill(18)(zero))
    )

    val expected =
      op(0x0102) :: op(0x0304) :: op(0x0506) :: op(0x0708) ::
        op(0x090A) :: op(0x0B0C) :: op(0x0D0E) :: op(0x0F10) ::
        op(0x9080) :: op(0x9070) :: op(0x9060) :: Nil

    IP6Address.opt(ValidLongStrings.Ascending).value should have(
      'octetPairs(expected)
    )
  }

  private object InvalidStrings {
    val TwelveDigits = "0:0:0:0:0:0:123456789AB:0"
    val TwoZeroGroupAbbreviations = "1::1::"
    val TrailingDot = "1:2:3:4:5:6:7:8."
    val NonNumeric = "1k:2:3:4:5:6:7:8"
    val InvalidSeparator = "1::0;1"
    val NonHexadecimalCharacters = "0::t"
  }

  it should "not be constructed from a invalid string representation" in {
    val validSuffix = ":1" * 7

    IP6Address.opt(" ") should be(None)
    IP6Address.opt(" " + ValidStrings.AllZeros) should be(None)
    IP6Address.opt("$" + validSuffix) should be(None)
    IP6Address.opt(InvalidStrings.TwelveDigits) should be(None)
    IP6Address.opt(InvalidStrings.TwoZeroGroupAbbreviations) should be(None)
    IP6Address.opt(InvalidStrings.TrailingDot) should be(None)
    IP6Address.opt(InvalidStrings.NonNumeric) should be(None)
    IP6Address.opt(InvalidStrings.NonHexadecimalCharacters) should be(None)
  }

  it should "not be constructed from a null string" in {
    IP6Address.opt(null) should be(None)
  }

  it should "not be constructed from an empty string" in {
    IP6Address.opt("") should be(None)
  }

  /* Pattern Matching */

  //  IP6Address has less enforcement in it's constructor than IP4Address
  //  and the extractor should follow this. As an API law for IP4 we have
  //  For each IP4Address the string repr should be matched to. Clearly the
  //  reverse relationship holds because the match results in an instance of
  //  IP6Address. If IP6Address can be constructed from null, empty and any
  //  non-empty string then the extractor should do the same.

  it should "be usable in string pattern matching" in {
    def m(x: String) = x match {
      case IP6Address(octetPairs) => octetPairs
      case _ => None
    }
    val eightZeroes = List.fill(8)(zero)

    m("") should be(None)
    m("f") should be(op(0x0, 0xf) :: Nil)
    /* Case insensitive */
    m("FA77") should be(op(0xfa, 0x77) :: Nil)
    /* Fewer than 8 octet pairs */
    m("0:5") should be(zero :: five :: Nil)
    m("0:1:2:3:4:5:6:7") should be(zero :: one :: two :: three :: four :: five :: six :: seven :: Nil)
    /* Invalid separator */
    m("0;1") should be(None)
    m("0:1:2:3:4:5;6:7") should be(None)
    m("55.66.77") should be(None)
    /* Non-hexadecimal value */
    m("0:t") should be(None)
    m("xx") should be(None)
    /* More than 8 octets */
    m("0:1:2:3:4:5:6:7:0:1") should be(
      zero :: one :: two :: three :: four :: five :: six :: seven :: zero :: one :: Nil)
    m("0000:0000:0000:0000:0000:0000:0000:0000") should be(eightZeroes)
    /* Not alternating between digits and separators */
    m("7:d:") should be(None)
    m(":7:d") should be(None)
    /* Zero groups abbreviations */
    m("::") should be(eightZeroes)
    m("0::") should be(eightZeroes)
    m("7::") should be(seven :: zero :: zero :: zero :: zero :: zero :: zero :: zero :: Nil)
    m("7::8") should be(seven :: zero :: zero :: zero :: zero :: zero :: zero :: eight :: Nil)
    /* Two abbreviations with fewer than 8 groups is ambiguous */
    m("::1:2::") should be(None)
    m("1::2::") should be(None)
    m("::1::2") should be(None)
    m("f::1::2") should be(None)
    m("::::") should be(None)
    m("1::::2") should be(None)
    m("f::1::2::3") should be(None)
    val ExpectedDescending = eight :: seven :: six :: five :: four :: three :: two :: one :: Nil
    val ExpectedDescendingPlus = eight :: seven :: six :: five :: four :: three :: two :: one :: eight :: six :: Nil
    /* Abbreviations that make sense as empty. This consistent with Rule 1. */
    m("::8:7:6:5:4:3:2:1") should be(ExpectedDescending)
    m("::8:7:6:5:4:3:2:1:8:6") should be(ExpectedDescendingPlus)
    m("8:7:6:5:4:3:2:1::") should be(ExpectedDescending)
    m("8:7:6:5:4:3:2:1:8:6::") should be(ExpectedDescendingPlus)
    m("8:7::6:5:4:3:2:1") should be(ExpectedDescending)
    m("8:7::6:5:4:3:2:1:8:6") should be(ExpectedDescendingPlus)

    /*
    * Multiple abbreviations that make sense as empty but we chose to drop on the basis there is
    * more information present than we can use. Some other extractor may be able to make better use of
    * the information. For example :: might be a structural separator for some other domain type.
    */
    /* leading and internal */
    m("::8:7:6:5:4::3:2:1") should be(None)
    m("::8:7:6:5:4::3:2:1:8:6") should be(None)
    /* internal and trailing */
    m("8:7:6:5:4::3:2:1::") should be(None)
    m("8:7:6:5:4::3:2:1:8:6::") should be(None)
    /* internal and internal */
    m("8:7:6::5:4::3:2::1") should be(None)
    m("8::7::6:5:4::3:2:1:8:6") should be(None)
    /* leading and trailing */
    m("::8:7:6:5:4:3:2:1::") should be(None)
    m("::8:7:6:5:4:3:2:1:8:6::") should be(None)
    /* leading, internal and trailing */
    m("::8:7:6:5::4:3:2:1::") should be(None)
    m("::8:7:6:5::4:3:2:1:8:6::") should be(None)
  }


  it should "convert a convertable unconstrained IP6Address to a constrained IP6Address" in {
    val unconstrained = IP6Address.opt("f00d:ca73::119").value

    unconstrained.constrained.value should have(
      'field1(constrainedOctetPair(0xf00d)),
      'field2(constrainedOctetPair(0xca73)),
      'field3(constrainedOctetPair(0)),
      'field4(constrainedOctetPair(0)),
      'field5(constrainedOctetPair(0)),
      'field6(constrainedOctetPair(0)),
      'field7(constrainedOctetPair(0)),
      'field8(constrainedOctetPair(0x0119))
    )

  }

  private val emptyOctetPair = OctetPair(None, None)

  private object UnconvertibleAddresses {
    val TooShort = IP6Address.opt("f001:ca72:113").value
    val TooLong = IP6Address.opt("f001:ca72:113:4:5:6:7:8:9").value
    val IncompleteOctetPair = IP6Address(zero :: emptyOctetPair :: two :: Nil)
  }

  it should "not convert unconvertable unconstrained IP6Addresses to a constrained IP6Addresses" in {
    UnconvertibleAddresses.TooShort.constrained should be(None)
    UnconvertibleAddresses.TooLong.constrained should be(None)
    UnconvertibleAddresses.IncompleteOctetPair.constrained should be(None)
  }

  private def op(hi: Int, lo: Int): OctetPair = OctetPair(Octet(hi), Octet(lo))

  private def op(v: Int): OctetPair = OctetPair(Octet(v / 0x100), Octet(v % 0x100))

  private def constrainedOctetPair(v: Int): ConstrainedOctetPair = ConstrainedOctetPair.opt(v).value


}
