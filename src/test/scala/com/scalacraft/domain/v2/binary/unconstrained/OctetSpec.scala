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

import com.scalacraft.domain.v2.binary.{Octet => Other}

/**
 * Specification for an unconstrained `Octet`
 */
class OctetSpec extends FlatSpec with Matchers {

  /* Greater than 255 */
  private object ExampleOctet {
    val Number = 0x4b1
    val String = "4b1"
  }

  private val ValidOctet = 201

  private val InvalidOctet = -1

  behavior of "An unconstrained Octet"

  /* Constructor args */

  it should "reject null constructor args" in {
    val thrown = the[NullConstructorArgumentException] thrownBy new Octet(null)
    thrown.paramName should equal("octet")
  }

  /* Pattern Matching */

  private object Unmatched

  it should "be usable in integer pattern matching" in {
    def m(x: Int) = x match {
      case Octet(n) => n
      case _ => Unmatched
    }
    m(5) should equal(Some(5))
    m(-5005) should equal(Some(-5005))
  }

  it should "be usable in string pattern matching" in {
    def m(x: String) = x match {
      case Octet(n) => n
      case _ => Unmatched
    }
    /* Rule 1 - Instances Have Alternative Representations */
    m(null) should be(None)
    m("") should be(None)
    m(" \t") should be(None)
    m("ff") should equal(Some(255))
    m(ExampleOctet.String) should equal(Some(ExampleOctet.Number))
    m("NaN") should be(Unmatched)
    m("41t") should be(Unmatched)
    /* Int is signed 4 bytes: 11223344 */
    /* 7fffffff */
    val maxHexInt = Int.MaxValue.formatted("%h")
    val maxHexIntPlusOne = "80000000"
    m("0fff") should equal(Some(0x0fff))
    m("0fffff") should equal(Some(0x0fffff))
    m("7fffffff") should equal(Some(0x7fffffff))
    m("11223344") should equal(Some(0x11223344))
    m(maxHexInt) should equal(Some(Int.MaxValue))
    m(maxHexIntPlusOne) should be(Unmatched)
    m("0") should equal(Some(0))
    m("00") should equal(Some(0))
    m("054") should equal(Some(0x54))
    m("0054") should equal(Some(0x54))
    m("07fffffff") should equal(Some(0x7fffffff))
    m("007fffffff") should equal(Some(0x7fffffff))
    /* Negative values */
    m("-1") should equal(Some(-0x1))
    /* -80000000 */
    val minHexInt = Int.MinValue.formatted("-%h")
    m(minHexInt) should equal(Some(Int.MinValue))
    val minHexIntMinusOne = "-80000001"
    m(minHexIntMinusOne) should be(Unmatched)
  }

  /* Implicit Conversions */

  it should "implicitly convert to some int" in {
    val octet = Octet(Some(ExampleOctet.Number))
    val i: Option[Int] = octet
    i.value should equal(ExampleOctet.Number)
  }

  it should "implicitly convert to none int" in {
    val octet = Octet(None)
    val i: Option[Int] = octet
    i should be(None)
  }

  it should "implicitly convert to some string" in {
    val octet = Octet(Some(ExampleOctet.Number))
    val s: Option[String] = octet
    s.value should equal(ExampleOctet.String)
  }

  it should "implicitly convert to some string when defined with more than 8 bits" in {
    val octet = Octet(Some(0x4321))
    val s: Option[String] = octet
    s.value should equal("4321")
  }

  it should "implicitly convert to none string" in {
    val octet = Octet(None)
    val s: Option[String] = octet
    s should be(None)
  }

  it should "implicitly convert to a constrained Octet when the octet is valid" in {
    val octet = Octet(Some(ValidOctet))
    val otherOpt: Option[Other] = octet
    otherOpt.value.octet should equal(ValidOctet)
  }

  it should "implicitly convert to None when octet is defined but invalid" in {
    val octet = Octet(Some(InvalidOctet))
    val otherOpt: Option[Other] = octet
    otherOpt should be(None)
  }

  it should "implicitly convert to None when octet is undefined" in {
    val octet = Octet(None)
    val otherOpt: Option[Other] = octet
    otherOpt should be(None)
  }
}
