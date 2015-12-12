/*
   Copyright 2014 Janek Bogucki

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

import com.scalacraft.domain.v2.internal.Reflections
import org.scalatest.FlatSpec

import org.scalatest.Matchers

import org.scalatest.OptionValues._

import com.scalacraft.domain.v2.net.unconstrained.{IP4Address => Unconstrained}

/**
 * Specification for `IP4Adress`
 */
class IP4AddressSpec extends FlatSpec with Matchers {

  private val ValidDottedQuad = (192, 168, 0, 1)

  private val FormattedValidDottedQuad = "192.168.0.1"

  private val InvalidByte = 256

  behavior of "An IP4Address"

  /* Construction from integers */

  it should "be constructed from valid inputs" in {
    val ip4Opt: Option[IP4Address] = IP4Address.opt(
      ValidDottedQuad._1,
      ValidDottedQuad._2,
      ValidDottedQuad._3,
      ValidDottedQuad._4
    )
    ip4Opt.value should have(
      'byte1(192),
      'byte2(168),
      'byte3(0),
      'byte4(1)
    )
  }

  it should "not be constructed from invalid input" in {
    val ip4Opt: Option[IP4Address] = IP4Address.opt(
      ValidDottedQuad._1,
      InvalidByte,
      ValidDottedQuad._3,
      ValidDottedQuad._4
    )
    ip4Opt should be(None)
  }

  /* Construction from a string */

  private object Strings {
    val ValidQuad = "240.1.255.7"
    val InvalidQuad = "240.1.255.B"
    val RangeExceededByte = "240.256.234.7"
    val TrailingDot = ValidQuad + "."
    val NonNumeric = "a.b.c.d"
  }

  it should "be constructed from a valid string representation" in {
    val ip4Opt = IP4Address.opt(Strings.ValidQuad)

    ip4Opt.value should have(
      'byte1(240),
      'byte2(1),
      'byte3(255),
      'byte4(7)
    )
  }

  it should "not be constructed from a invalid string representation" in {
    IP4Address.opt(Strings.RangeExceededByte) should be(None)
    IP4Address.opt(Strings.TrailingDot) should be(None)
    IP4Address.opt(Strings.NonNumeric) should be(None)
  }

  it should "not be constructed from a null string" in {
    IP4Address.opt(null: String) should be(None)
  }

  it should "not be constructed from an empty string" in {
    IP4Address.opt("") should be(None)
  }

  /* Constructor access */

  it should "not have a public constructor" in {
    val constructors = Reflections.declaredConstructors[IP4Address]
    constructors should have size 1
    val con = constructors.head
    con shouldBe 'private
  }

  it should "not allow direct instantiation" in {
    val (b1, b2, b3, b4) = ValidDottedQuad
    "new IP4Address(b1, b2, b3, b4)" shouldNot compile
  }

  /* Pattern Matching */

  it should "be usable in string pattern matching" in {
    def m(x: String) = x match {
      case IP4Address(b1, b2, b3, b4) => Some(b1, b2, b3, b4)
      case _ => None
    }
    m(Strings.InvalidQuad) should be(None)
    m(Strings.NonNumeric) should be(None)
    m(Strings.ValidQuad).value should equal((240, 1, 255, 7))
  }

  /* Implicit Conversions */

  it should "implicitly convert to a string" in {
    val ip4Opt: Option[IP4Address] = IP4Address.opt(
      ValidDottedQuad._1,
      ValidDottedQuad._2,
      ValidDottedQuad._3,
      ValidDottedQuad._4
    )
    val s: String = ip4Opt.value
    /* n.n.n.n */
    s should equal(FormattedValidDottedQuad)
  }

  it should "implicitly convert to an unconstrained IP4Address" in {
    val ipa: IP4Address = IP4Address.opt(88, 0, 2, 119).value
    val other: Unconstrained = ipa
    other should have(
      'byte1(88),
      'byte2(0),
      'byte3(2),
      'byte4(119)
    )
  }

  it should "convert to an unconstrained IP4Address" in {
    val constrained = IP4Address.opt(88, 0, 2, 119).value
    val unconstrained: Unconstrained = constrained.unconstrained
    unconstrained should have(
      'byte1(88),
      'byte2(0),
      'byte3(2),
      'byte4(119)
    )
  }
}
