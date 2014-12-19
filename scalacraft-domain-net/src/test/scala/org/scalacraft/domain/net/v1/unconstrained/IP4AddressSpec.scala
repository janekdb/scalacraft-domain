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
package org.scalacraft.domain.net.v1.unconstrained

import org.scalatest.FlatSpec

import org.scalatest.Matchers
import org.scalatest.OptionValues._

import org.scalacraft.domain.net.v1.{IP4Address => Other}

/**
 * Specification for an unconstrained `IP4AddressSpec`
 */
class IP4AddressSpec extends FlatSpec with Matchers {

  behavior of "An IP4Address"

  /* Pattern Matching */

  it should "be usable in string pattern matching" in {
    def m(x: String) = x match {
      case IP4Address(b1, b2, b3, b4) => (b1, b2, b3, b4)
      case _ => None
    }
    m("55.66.77") should be(None)
    m("xx") should be(None)
    m("55.66.77.88") should equal((55, 66, 77, 88))
    m("550.66.77.88") should equal((550, 66, 77, 88))
    /* Valid Int min and max */
    m("1.1.0.2147483647") should equal((1, 1, 0, 2147483647))
    m("1.1.-2147483648.0") should equal((1, 1, -2147483648, 0))
    m("1.1.0.2147483648") should be(None)
    m("1.1.-2147483649.0") should be(None)
  }

  /* Implicit Conversions */

  it should "implicitly convert to a string" in {
    val ipa = IP4Address(1, -2, 3, 555)
    val s: String = ipa
    s should equal("1.-2.3.555")
  }

  it should "implicitly convert to a constrained IP4AddressSpec when the address is valid" in {
    val ipa = IP4Address(1, 2, 4, 5)
    val otherOpt: Option[Other] = ipa
    otherOpt.value should have(
      'byte1(1),
      'byte2(2),
      'byte3(4),
      'byte4(5)
    )
  }

  it should "implicitly convert to None when address is invalid" in {
    val ipa = IP4Address(999, 0, 0, 0)
    val otherOpt: Option[Other] = ipa
    otherOpt should be(None)
  }

}
