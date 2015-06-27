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

/**
 * Specification for `Port`
 */
class PortSpec extends FlatSpec with Matchers {

  private val ValidPortNumber = 5511

  private val InvalidPortNumber = -129

  private val MaxPortNumber = 65535

  behavior of "A Port"

  /* Construction from an integer */

  it should "be constructed from a valid port number" in {
    val portOpt: Option[Port] = Port.opt(ValidPortNumber)
    portOpt.value.portNumber should equal(ValidPortNumber)
  }

  it should "not be constructed from an invalid port number" in {
    Port.opt(InvalidPortNumber) should be(None)
  }

  it should "not have one-off errors" in {
    Port.opt(-1) should be(None)
    Port.opt(0).value.portNumber should equal(0)
    Port.opt(65535).value.portNumber should equal(65535)
    Port.opt(65535 + 1) should be(None)
  }

  it should "be usable in pattern matching" in {
    def m(x: Int) = x match {
      case Port(p) => p
      case _ => None
    }
    m(5) should equal(5)
    m(-1) should be(None)
  }

  /* Construction from a string */

  it should "be constructed from a valid port number string" in {
    val portOpt: Option[Port] = Port.opt(ValidPortNumber.toString)
    portOpt.value.portNumber should equal(ValidPortNumber)
  }

  it should "not be constructed from an invalid port number string" in {
    Port.opt(InvalidPortNumber.toString) should be(None)
  }

  it should "not be constructed from a null string" in {
    Port.opt(null: String) should be(None)
  }

  it should "not be constructed from an empty string" in {
    Port.opt("") should be(None)
  }

  it should "not be constructed from a non-numeric string" in {
    Port.opt("3rd") should be(None)
    Port.opt("0xff") should be(None)
    Port.opt("10.9") should be(None)
  }

  it should "not be constructed from an out of range positive integer string" in {
    val n = MaxPortNumber + 1
    Port.opt(n.toString) should be(None)
  }

  it should "not be constructed from a negative integer string" in {
    val n = -1
    Port.opt(n.toString) should be(None)
  }

  /* Constructor access */

  it should "not have a public constructor" in {
    val constructors = Reflections.declaredConstructors[Port]
    constructors should have size 1
    val con = constructors.head
    con.isPrivate should be(true)
  }

  it should "not allow direct instantiation" in {
    "new Port(9418)" shouldNot compile
  }

  /* Pattern Matching */

  it should "be usable in string pattern matching" in {
    def m(x: String) = x match {
      case Port(p) => p
      case _ => None
    }
    m(ValidPortNumber.toString) should equal(ValidPortNumber)
    m(InvalidPortNumber.toString) should be(None)
  }

  /* Implicit Conversions */

  it should "implicitly convert to an int" in {
    val Some(port) = Port.opt(ValidPortNumber)
    val s: Int = port
    s should equal(ValidPortNumber)
  }

  it should "implicitly convert to a string" in {
    val Some(port) = Port.opt(ValidPortNumber)
    val s: String = port
    s should equal(ValidPortNumber.toString)
  }

  it should "implicitly convert to an unconstrained Port" in {
    val Some(port) = Port.opt(ValidPortNumber)
    val uncons: unconstrained.Port = port
    uncons.portNumber should equal(ValidPortNumber)
  }
}
