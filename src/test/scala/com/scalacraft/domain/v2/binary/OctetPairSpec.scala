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
package com.scalacraft.domain.v2.binary

import com.scalacraft.domain.v2.binary.unconstrained.{OctetPair => Unconstrained}
import com.scalacraft.domain.v2.internal.Reflections

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.OptionValues._

/**
 * Specification for `OctetPair`
 */
class OctetPairSpec extends FlatSpec with Matchers {

  private implicit class O(x: Int) {
    def o: Octet = Octet.opt(x).get
  }

  private val Octet1 = Octet.opt(0x10).get
  private val Octet2 = Octet.opt(0x81).get

  behavior of "An OctetPair"

  /* Construction from Octets */

  it should "be constructed from non-null inputs" in {
    val octetPairOpt: Option[OctetPair] = OctetPair.opt(Octet1, Octet2)
    octetPairOpt.value should have(
      'hi(Octet1),
      'lo(Octet2)
    )
  }

  it should "not be constructed when any input is null" in {
    OctetPair.opt(null, Octet2) should be(None)
    OctetPair.opt(Octet1, null) should be(None)
    OctetPair.opt(null, null) should be(None)
  }

  /* Construction from a string */

  it should "be constructed from a valid octet pair number string" in {
    OctetPair.opt("5").value should have(
      'hi(0.o),
      'lo(5.o)
    )
    OctetPair.opt("a5").value should have(
      'hi(0.o),
      'lo(0xa5.o)
    )
    OctetPair.opt("b75").value should have(
      'hi(0xb.o),
      'lo(0x75.o)
    )
    OctetPair.opt("f00f").value should have(
      'hi(0xf0.o),
      'lo(0xf.o)
    )
  }

  it should "not be constructed from a null string" in {
    OctetPair.opt(null: String) should be(None)
  }

  it should "not be constructed from an empty string" in {
    OctetPair.opt("") should be(None)
    OctetPair.opt(" ") should be(None)
  }

  it should "not be constructed from an invalid numeric string" in {
    OctetPair.opt("10.9") should be(None)
  }

  it should "not be constructed from a non-numeric string" in {
    OctetPair.opt("-") should be(None)
    OctetPair.opt("3rd") should be(None)
    OctetPair.opt("0xff") should be(None)
    OctetPair.opt("g") should be(None)
  }

  it should "not be constructed from an out of range valid numeric string" in {
    /* 0x91235 */
    OctetPair.opt("91235") should be(None)
    OctetPair.opt("-1") should be(None)
    OctetPair.opt("-9") should be(None)
    OctetPair.opt("-fa") should be(None)
  }

  /* Construction from an int */

  it should "be constructed from an in range int" in {
    OctetPair.opt(0x0).value should have(
      'hi(0.o),
      'lo(0.o)
    )
    OctetPair.opt(0x5).value should have(
      'hi(0.o),
      'lo(5.o)
    )
    OctetPair.opt(0xa5).value should have(
      'hi(0.o),
      'lo(0xa5.o)
    )
    OctetPair.opt(0xb75).value should have(
      'hi(0xb.o),
      'lo(0x75.o)
    )
    OctetPair.opt(0xf00f).value should have(
      'hi(0xf0.o),
      'lo(0xf.o)
    )
    OctetPair.opt(0xffff).value should have(
      'hi(0xff.o),
      'lo(0xff.o)
    )
  }

  it should "not be constructed from an out of range int" in {
    OctetPair.opt(-1) should be(None)
    OctetPair.opt(256 * 256) should be(None)
  }

  /* Constructor access */

  it should "not have a public constructor" in {
    val constructors = Reflections.declaredConstructors[OctetPair]
    constructors should have size 1
    val con = constructors.head
    con shouldBe 'private
  }

  it should "not allow direct instantiation" in {
    val Some(pair) = OctetPair.opt(0x0)
    val hi = pair.hi
    val lo = pair.lo
    "new OctetPair(hi, lo)" shouldNot compile
  }

  /* Pattern Matching */

  it should "be usable in integer pattern matching" in {
    def m(x: Int) = x match {
      case OctetPair(hi, lo) => (hi.octet, lo.octet)
      case _ => None
    }
    m(5) should equal((0, 5))
    m(255) should equal((0, 255))
    m(256) should equal((1, 0))
    m(-1) should be(None)
    m(0xffff) should equal((255, 255))
    m(0x10000) should be(None)
  }

  private val ZeroOctet = Octet.Zero

  it should "be usable in string pattern matching" in {
    def m(x: String) = x match {
      case OctetPair(a, b) => (a, b)
      case _ => None
    }
    m(null) should be(None)
    m("") should be(None)
    m(" " * 2) should be(None)
    m("0") should equal((ZeroOctet, ZeroOctet))
    m("0d") should equal((ZeroOctet, 0xd.o))
    m("00d") should equal((ZeroOctet, 0xd.o))
    m("50d") should equal((0x5.o, 0xd.o))
    m("050d") should equal((0x5.o, 13.o))
    m("250d") should equal((0x25.o, 13.o))
    m("0102") should equal((1.o, 2.o))
    m("f") should equal((ZeroOctet, 15.o))
    m("e1") should equal((ZeroOctet, 0xe1.o))
    m("eh") should be(None)
    m("161") should equal((1.o, 0x61.o))
    m("12233") should be(None)
    m("p7788") should be(None)
    m("d") should equal((ZeroOctet, 0xd.o))
    m("1d") should equal((ZeroOctet, 0x1d.o))
    m("51d") should equal((0x5.o, 0x1d.o))
    m("251d") should equal((0x25.o, 0x1d.o))
    m("4251d") should be(None)
  }

  /* Implicit Conversions */

  it should "implicitly convert to an int" in {
    val Some(octetPair) = OctetPair.opt(0x12fe)
    val s: Int = octetPair
    s should equal(0x12fe)
  }

  it should "implicitly convert to an int with addition" in {
    val Some(pair) = OctetPair.opt(0x40cc)
    val w: Int = 1 + pair
    w should equal(0x40cd)
  }

  it should "implicitly convert to a string from a valid int" in {
    val Some(octetPair) = OctetPair.opt(0x12fe)
    val s: String = octetPair
    s should equal("12fe")
  }

  it should "implicitly convert to a string from a valid low int" in {
    val Some(octetPair) = OctetPair.opt(0xf)
    val s: String = octetPair
    s should equal("000f")
  }

  it should "implicitly convert to an unconstrained Octet with hi and lo" in {
    val Some(octetPair) = OctetPair.opt(0x1122)
    val uncons: Unconstrained = octetPair
    uncons.hi.value.octet should equal(Some(0x11))
    uncons.lo.value.octet should equal(Some(0x22))
  }

  it should "implicitly convert to an unconstrained Octet with lo only" in {
    val Some(octetPair) = OctetPair.opt(0x22)
    val uncons: Unconstrained = octetPair
    uncons.hi.value.octet should equal(Some(0x00))
    uncons.lo.value.octet should equal(Some(0x22))
  }
}
