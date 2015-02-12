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

import com.scalacraft.domain.v2.country.{CountryCodeNumeric => Other}
import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.OptionValues._

/**
 * Specification for an unconstrained `CountryCodeNumeric`
 */
class CountryCodeNumericSpec extends FlatSpec with Matchers {

  private val ValidCountryCode = "709"

  /* Not three digits */
  private val InvalidCountryCode = "6K7"

  behavior of "An unconstrained CountryCodeNumeric"

  /* Pattern Matching */

  // TODO: Understand why m(x: Any) causes this test to fail
  it should "be usable in pattern matching" in {
    def m(x: String) = x match {
      case CountryCodeNumeric(cc) => cc
      case _ => None
    }
    m("") should be("")
    m("1") should be("1")
    m("22") should equal("22")
    m("431") should be("431")
    m("8899-") should be("8899-")
    m("POL") should be("POL")
  }

  /* Implicit Conversions */

  it should "implicitly convert to a string" in {
    val cc = CountryCodeNumeric(ValidCountryCode)
    val s: String = cc
    s should equal(ValidCountryCode)
  }

  it should "implicitly convert to a constrained CountryCodeNumeric when the code is valid" in {
    val cc = CountryCodeNumeric(ValidCountryCode)
    val otherOpt: Option[Other] = cc
    otherOpt.value.countryCode should equal(ValidCountryCode)
  }

  it should "implicitly convert to None when the code is invalid" in {
    val cc = CountryCodeNumeric(InvalidCountryCode)
    val otherOpt: Option[Other] = cc
    otherOpt should be(None)
  }
}
