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
package com.scalacraft.domain.v2.country.unconstrained

import com.scalacraft.domain.v2.country.{CountryCodeA3 => Other}
import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.OptionValues._

/**
 * Specification for an unconstrained `CountryCodeA3`
 */
class CountryCodeA3Spec extends FlatSpec with Matchers {

  private val ValidCountryCode = "AZE"

  /* Not three characters */
  private val InvalidCountryCode = "AZEE"

  behavior of "An unconstrained CountryCodeA3"

  /* Pattern Matching */

  // TODO: Understand why m(x: Any) causes this test to fail
  it should "be usable in pattern matching" in {
    def m(x: String) = x match {
      case CountryCodeA3(cc) => cc
      case _ => None
    }
    m("") should be("")
    m("P") should be("P")
    m("PL") should equal("PL")
    m("POL") should be("POL")
    m("POLS") should be("POLS")
  }

  /* Implicit Conversions */

  it should "implicitly convert to a string" in {
    val cc = CountryCodeA3(ValidCountryCode)
    val s: String = cc
    s should equal(ValidCountryCode)
  }

  it should "implicitly convert to a constrained CountryCodeA3 when the code is valid" in {
    val cc = CountryCodeA3(ValidCountryCode)
    val otherOpt: Option[Other] = cc
    otherOpt.value.countryCode should equal(ValidCountryCode)
  }

  it should "implicitly convert to None when the code is invalid" in {
    val cc = CountryCodeA3(InvalidCountryCode)
    val otherOpt: Option[Other] = cc
    otherOpt should be(None)
  }
}
