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

import com.scalacraft.domain.v2.internal.Reflections
import org.scalatest.{FlatSpec, Matchers}

import org.scalatest.OptionValues._

/**
 * Specification for `Octet`
 */
class OctetSpec extends FlatSpec with Matchers {

  private object ValidOctet {
    val Number = 254
    val HexString = "fe"
  }

  private val InvalidOctetNumber = -9

  private val MaxOctetValue = 255

  behavior of "An Octet"

  /* Construction from an integer */

  it should "be constructed from a valid octet number" in {
    val octetOpt: Option[Octet] = Octet.opt(ValidOctet.Number)
    octetOpt.value.octet should equal(ValidOctet.Number)
  }

  it should "not be constructed from an invalid octet number" in {
    Octet.opt(InvalidOctetNumber) should be(None)
  }

  it should "not have one-off errors" in {
    Octet.opt(-1) should be(None)
    Octet.opt(0).value.octet should equal(0)
    Octet.opt(MaxOctetValue).value.octet should equal(MaxOctetValue)
    Octet.opt(MaxOctetValue + 1) should be(None)
  }

  it should "be usable in pattern matching" in {
    def m(x: Int) = x match {
      case Octet(p) => p
      case _ => None
    }
    m(5) should equal(5)
    m(255) should equal(255)
    m(256) should be(None)
    m(-1) should be(None)
  }

  /* Construction from a string */

  it should "be constructed from a valid octet number string" in {
    Octet.opt("5").value.octet should equal(0x5)
    Octet.opt("f").value.octet should equal(0xf)
    Octet.opt("F").value.octet should equal(0xf)
    Octet.opt("54").value.octet should equal(0x54)
    Octet.opt("fe").value.octet should equal(0xfe)
    Octet.opt("FE").value.octet should equal(0xfe)
  }

  it should "not be constructed from a null string" in {
    Octet.opt(null: String) should be(None)
  }

  it should "not be constructed from an empty string" in {
    Octet.opt("") should be(None)
  }

  it should "not be constructed from an invalid numeric string" in {
    Octet.opt("10.9") should be(None)
    /* Valid hex but invalid for Octet */
    Octet.opt("fff") should be(None)
    Octet.opt("-f") should be(None)
  }

  it should "not be constructed from a non-numeric string" in {
    Octet.opt("-") should be(None)
    Octet.opt("3rd") should be(None)
    Octet.opt("0xff") should be(None)
    Octet.opt("g") should be(None)
  }

  it should "not be constructed from an out of range valid numeric string" in {
    /* 0x100 */
    Octet.opt("100") should be(None)
    Octet.opt("147") should be(None)
    Octet.opt("-1") should be(None)
    Octet.opt("-9") should be(None)
    Octet.opt("-fa") should be(None)
  }

  /* Constructor access */

  it should "not have a public constructor" in {
    val constructors = Reflections.declaredConstructors[Octet]
    constructors should have size (1)
    val con = constructors.head
    con.isPrivate should be(true)
  }

  /* Pattern Matching */

  it should "be usable in string pattern matching" in {
    def m(x: String) = x match {
      case Octet(p) => p
      case _ => None
    }
    m(ValidOctet.HexString) should equal(ValidOctet.Number)
    m("0") should equal(0)
    m("d") should equal(13)
    m("e1") should equal(0xe1)
    m("eh") should be(None)
    m("100") should be(None)
    m("255") should be(None)
  }

  /* Implicit Conversions */

  it should "implicitly convert to an int" in {
    val Some(octet) = Octet.opt(ValidOctet.Number)
    val s: Int = octet
    s should equal(ValidOctet.Number)
  }

  it should "implicitly convert to a string from a valid int" in {
    val Some(octet) = Octet.opt(ValidOctet.Number)
    val s: String = octet
    s should equal(ValidOctet.HexString)
  }

  // TODO
  //  it should "implicitly convert to an unconstrained Octet" in {
  //    val Some(octet) = Octet.opt(ValidOctet.Number)
  //    fail("TODO")
  //    val uncons: unconstrained.Octet = octet
  //    uncons.octetNumber should equal(ValidOctetNumber)
  //  }
}
