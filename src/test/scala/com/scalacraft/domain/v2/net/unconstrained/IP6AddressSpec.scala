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

import com.scalacraft.domain.v2.internal.ex.NullConstructorArgumentException
import com.scalacraft.domain.v2.internal.ex.NullElementException
import com.scalacraft.domain.v2.binary.unconstrained.{Octet, OctetPair}

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

  private def op(hi: Int, lo: Int): OctetPair = OctetPair(Octet(hi), Octet(lo))

  private def op(v: Int): OctetPair = OctetPair(Octet(v / 0x100), Octet(v % 0x100))
}
