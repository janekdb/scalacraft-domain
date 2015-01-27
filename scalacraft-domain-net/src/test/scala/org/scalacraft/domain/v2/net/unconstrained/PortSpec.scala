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
package org.scalacraft.domain.v2.net.unconstrained

import org.scalatest.FlatSpec

import org.scalatest.Matchers
import org.scalatest.OptionValues._

import org.scalacraft.domain.v2.net.{Port => Other}

/**
 * Specification for an unconstrained `Port`
 */
class PortSpec extends FlatSpec with Matchers {

  /* Greater than 65535 */
  private val PortNumber = 551140

  private val ValidPortNumber = 4580

  private val InvalidPortNumber = -1

  behavior of "An unconstrained Port"

  /* Pattern Matching */

  it should "be usable in pattern matching" in {
    def m(x: Int) = x match {
      case Port(p) => p
      case _ => None
    }
    m(5) should equal(5)
  }

  it should "be usable in string pattern matching" in {
    def m(x: String) = x match {
      case Port(p) => p
      case _ => None
    }
    m(PortNumber.toString) should equal(PortNumber)
  }

  /* Implicit Conversions */

  it should "implicitly convert to an int" in {
    val port = Port(PortNumber)
    val i: Int = port
    i should equal(PortNumber)
  }

  it should "implicitly convert to a string" in {
    val port = Port(PortNumber)
    val s: String = port
    s should equal(PortNumber.toString)
  }

  it should "implicitly convert to a constrained Port when the port is valid" in {
    val port = Port(ValidPortNumber)
    val otherOpt: Option[Other] = port
    otherOpt.value.portNumber should equal(ValidPortNumber)
  }

  it should "implicitly convert to None when port is invalid" in {
    val port = Port(InvalidPortNumber)
    val otherOpt: Option[Other] = port
    otherOpt should be(None)
  }
}
