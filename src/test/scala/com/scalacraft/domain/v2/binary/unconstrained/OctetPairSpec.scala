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
package com.scalacraft.domain.v2.binary.unconstrained

import com.scalacraft.domain.v2.internal.ex.NullConstructorArgumentException
import org.scalatest.{Matchers, FlatSpec}

import org.scalatest.OptionValues._

import com.scalacraft.domain.v2.binary.{OctetPair => Constrained}

/**
 * Specification for an unconstrained `OctetPair`
 */
class OctetPairSpec extends FlatSpec with Matchers {

  private implicit class O(x: Int) {
    def o: Some[Octet] = Some(Octet(Some(x)))
  }

  behavior of "An unconstrained OctetPair"

  private val Octet1 = Octet(1)

  private val Octet2 = Octet(2)

  /* Constructor args */

  it should "reject null constructor args" in {

    the[NullConstructorArgumentException] thrownBy {
      new OctetPair(null, None)
    } should have('paramName("hi"))

    the[NullConstructorArgumentException] thrownBy {
      new OctetPair(None, null)
    } should have('paramName("lo"))

    a[NullConstructorArgumentException] should be thrownBy new OctetPair(null, null)
  }

  it should "accept option constructor args" in {
    val octetPair = OctetPair(Some(Octet1), Some(Octet2))
    octetPair.hi.value.octet.value should equal(1)
    octetPair.lo.value.octet.value should equal(2)
  }

  it should "accept direct constructor args" in {
    OctetPair(Octet1, Octet2) should have(
      'hi(Some(Octet1)),
      'lo(Some(Octet2))
    )
    OctetPair(null, Octet2) should have(
      'hi(None),
      'lo(Some(Octet2))
    )
    OctetPair(Octet1, null) should have(
      'hi(Some(Octet1)),
      'lo(None)
    )
  }

  /* Pattern Matching */

  private object Unmatched

  it should "be usable in integer pattern matching" in {
    def m(x: Int) = x match {
      case OctetPair(hi, lo) => (hi, lo)
      case _ => Unmatched
    }
    m(5) should be(0.o, 5.o)
    m(255) should be((0.o, 255.o))
    m(256) should be((1.o, 0.o))
    m(-1) should be((0.o, (-1).o))
    m(-256) should be(((-1).o, 0.o))
    m(-257) should be(((-1).o, (-1).o))
    m(-0x45fa) should be(((-0x45).o, (-0xfa).o))
    m(0xffff) should be((0xff.o, 0xff.o))
    m(0x10000) should be((0x100.o, 0.o))
    m(Int.MinValue) should be((-0x800000.o, 0x00.o))
    m(Int.MaxValue) should be((0x7fffff.o, 0xff.o))
  }

  it should "be usable in string pattern matching" in {
    /* Int is signed 4 bytes: 11223344 */

    /* 4 bytes */
    /* 7fffffff */
    val maxFourByteHexInt = Int.MaxValue.formatted("%h")
    maxFourByteHexInt should equal("7fffffff")
    val maxFourByteHexIntPlusOne = "80000000"
    /* 5 bytes */
    val maxFiveByteHexInt = "807ffffeff" // 551903297279
    val maxFiveByteHexIntPlusOne = "807fffff00" // 551903297280

    /* 4 bytes */
    /* -80000000 */
    val minFourByteHexInt = Int.MinValue.formatted("-%h")
    minFourByteHexInt should equal("-80000000")
    val minFourByteHexIntMinusOne = "-80000001"
    /* 5 bytes */
    val minFiveByteHexInt = "-8080000000" // -551903297536
    val minFiveByteHexIntMinusOne = "-8080000001" // -551903297537

    def m(x: String) = x match {
      case OctetPair(a, b) => (a, b)
      case _ => None
    }
    m(null) should be(None)
    m("") should be(None)
    m(" " * 2) should be(None)
    m("-") should be(None)
    m("- ") should be(None)
    m(" -") should be(None)
    m(" - ") should be(None)
    m("- 7") should be(None)
    /* 0 defines both hi and lo */
    m("0") should equal((0.o, 0.o))
    m("0d") should equal((0.o, 0xd.o))
    m("00d") should equal((0.o, 0xd.o))
    m("50d") should equal((0x5.o, 0xd.o))
    m("050d") should equal((0x5.o, 13.o))
    m("250d") should equal((0x25.o, 13.o))
    m("0102") should equal((1.o, 2.o))
    m("CAF3") should equal((0xca.o, 0xf3.o))
    m("f") should equal((0.o, 15.o))
    m("e1") should equal((0.o, 0xe1.o))
    m("K") should be(None)
    m("eh") should be(None)
    m("161") should equal((1.o, 0x61.o))
    /* Confirm information that cannot be converted without loss is not used. */
    m("k161") should equal(None)
    m("12233") should equal((0x122.o, 0x33.o))
    m(maxFourByteHexInt) should equal((0x7fffff.o, 0xff.o))
    m(maxFourByteHexIntPlusOne) should equal((0x800000.o, 0x00.o))
    m(maxFiveByteHexInt) should equal((0x7fffffff.o, 0x7fffffff.o))
    m(maxFiveByteHexIntPlusOne) should be(None)
    /* aabbccdd cannot be used for the hi byte so this data should not be consumed. */
    m("aabbccddee") should be(None)
    m("p7788") should be(None)
    m("d") should equal((0.o, 0xd.o))
    m("1d") should equal((0.o, 0x1d.o))
    m("UT") should be(None)
    m("51d") should equal((0x5.o, 0x1d.o))
    m("251d") should equal((0x25.o, 0x1d.o))
    m("4251d") should equal((0x425.o, 0x1d.o))
    /* Less than zero cases */
    m("-0") should equal((0.o, 0.o))
    m("-1") should equal((0.o, (-1).o))
    m("-ff") should equal((0.o, (-0xff).o))
    m("-100") should equal(((-1).o, 0.o))
    m("-101") should equal(((-1).o, (-1).o))
    /* Less than zero edge cases */
    m(minFourByteHexInt) should equal((-0x800000.o, 0x00.o))
    m(minFourByteHexIntMinusOne) should equal((-0x800000.o, (-1).o))
    m(minFiveByteHexInt) should equal((-0x80000000.o, -0x80000000.o))
    m(minFiveByteHexIntMinusOne) should be(None)
    /* Hex prefix does not match */
    m("0x01") should be(None)
  }

  /* Implicit Conversions */

  //  private type Octets = (Option[Octet], Option[Octet])

  //  it should "implicitly convert to a tuple of some octets" in {
  //    val lo = Octet(Some(0x01))
  //    val hi = Octet(Some(0x2000))
  //    val octets: Octets = new OctetPair(Some(hi), Some(lo))
  //    octets should equal(Some(hi), Some(lo))
  //  }


  //  it should "test pattern matching" in {
  //    val lo = Octet(Some(0x01))
  //    val hi = Octet(Some(0x2000))
  //    val op = new OctetPair(Some(hi), Some(lo))
  //    val OctetPair(hi2, lo2)  = op
  ////    octets should equal(Some(hi), Some(lo))
  //  }

  //
  //  it should "implicitly convert to none int" in {
  //    val octet = Octet(None)
  //    val i: Option[Int] = octet
  //    i should be(None)
  //  }

  it should "have an implicit conversion to String equal to none from none + none" in {
    val octetPair = OctetPair(None, None)
    val s: Option[String] = octetPair
    s should be(None)
  }

  it should "have an implicit conversion to String equal to none from none + some" in {
    val octetPair = OctetPair(None, Some(Octet(0x17)))
    val s: Option[String] = octetPair
    s should be(None)
  }

  it should "have an implicit conversion to String equal to none from some + none" in {
    val octetPair = OctetPair(Some(Octet(0x17)), None)
    val s: Option[String] = octetPair
    s should be(None)
  }

  it should "have an implicit conversion to String equal to some from some + some" in {
    val octetPair = OctetPair(Some(Octet(0xab)), Some(Octet(0x17)))
    val s: Option[String] = octetPair
    s.value should be("ab17")
  }

  it should "have an implicit conversion to String equal to some with padding from some + some" in {
    val octetPair = OctetPair(Some(Octet(0x00)), Some(Octet(0x0c)))
    val s: Option[String] = octetPair
    s.value should be("000c")
  }

  it should "have an implicit conversion to String equal to none when each octet is larger than 0xff" in {
    val octetPair = OctetPair(Some(Octet(0x77abcd00)), Some(Octet(0x1734)))
    val s: Option[String] = octetPair
    s.value should be("77abcd1734")
  }

  it should "have an implicit conversion to constrained OctetPair when the octets are valid" in {
    val hi = Octet(Some(0xdd))
    val lo = Octet(Some(0x11))
    val octetPair = OctetPair(hi, lo)
    val otherOpt: Option[Constrained] = octetPair
    otherOpt.value.lo.octet should be(0x11)
    otherOpt.value.hi.octet should be(0xdd)
  }

  it should "have an implicit conversion to constrained OctetPair equal to none when the lo octet has an undefined value" in {
    val hi = Octet(Some(0xdd))
    val lo = Octet(None)
    val octetPair = OctetPair(hi, lo)
    val otherOpt: Option[Constrained] = octetPair
    otherOpt should be(None)
  }

  it should "have an implicit conversion to constrained OctetPair equal to none when the lo octet is undefined" in {
    val hi = Some(Octet(Some(0xdd)))
    val lo = None
    val octetPair = OctetPair(hi, lo)
    val otherOpt: Option[Constrained] = octetPair
    otherOpt should be(None)
  }

  it should "have an implicit conversion to constrained OctetPair equal to none when the hi octet has an undefined value" in {
    val hi = Octet(None)
    val lo = Octet(Some(0x11))
    val octetPair = OctetPair(hi, lo)
    val otherOpt: Option[Constrained] = octetPair
    otherOpt should be(None)
  }

  it should "have an implicit conversion to constrained OctetPair equal to one when the hi octet is undefined" in {
    val hi = None
    val lo = Some(Octet(Some(0x11)))
    val octetPair = OctetPair(hi, lo)
    val otherOpt: Option[Constrained] = octetPair
    otherOpt should be(None)
  }

  it should "have an implicit conversion to constrained OctetPair equal to none when either octet is out of range" in {
    val hi = Octet(Some(0xdd))
    val lo = Octet(Some(0x22))
    val octetPair = OctetPair(hi, lo)
    def otherOpt(octetPair: OctetPair): Option[Constrained] = octetPair
    otherOpt(OctetPair(hi, lo)) should be('defined)
    otherOpt(OctetPair(Octet(Some(-1)), lo)) should be(None)
    otherOpt(OctetPair(Octet(Some(0x100)), lo)) should be(None)
    otherOpt(OctetPair(hi, Octet(Some(-1)))) should be(None)
    otherOpt(OctetPair(hi, Octet(Some(0x100)))) should be(None)
  }
}
