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
package com.scalacraft.domain.v2.country

import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.OptionValues._

/**
 * Specification for `CountryA2`
 */
class CountryCodeA2Spec extends FlatSpec with Matchers {

  private val ValidCountryCode = "SC"

  private val ShortCountryCode = "L"

  private val LongCountryCode = ValidCountryCode * 2

  private val InvalidCharsCountryCode = "8T"

  private val LowerCaseCountryCode = ValidCountryCode.toLowerCase

  behavior of "A CountryCodeA2Spec"

  /* Construction from a string */

  it should "be constructed from a valid country code" in {
    val ccOpt: Option[CountryCodeA2] = CountryCodeA2.opt(ValidCountryCode)
    ccOpt.value.countryCode should equal(ValidCountryCode)
  }

  it should "not be constructed from a short country code" in {
    CountryCodeA2.opt(ShortCountryCode) should be(None)
  }

  it should "not be constructed from a long country code" in {
    CountryCodeA2.opt(LongCountryCode) should be(None)
  }

  it should "not be constructed from a country code with invalid character" in {
    CountryCodeA2.opt(InvalidCharsCountryCode) should be(None)
  }

  it should "not be constructed from a lowercase country code" in {
    CountryCodeA2.opt(LowerCaseCountryCode) should be(None)
  }

  /* Constructor access */

  //  it should "not have a public constructor" in (pending)

  /* Pattern Matching */

  it should "be usable in string pattern matching" in {
    def m(x: String) = x match {
      case CountryCodeA2(cc) => cc
      case _ => None
    }
    m(ValidCountryCode) should equal(ValidCountryCode)
    m(InvalidCharsCountryCode) should be(None)
  }

  /* Implicit Conversions */

  it should "implicitly convert to a string" in {
    val Some(cc) = CountryCodeA2.opt(ValidCountryCode)
    val s: String = cc
    s should equal(ValidCountryCode)
  }

  //  it should "implicitly convert to an unconstrained Port" in (pending)
  //  {
  //    val Some(port) = Port.opt(ValidPortNumber)
  //    val uncons: unconstrained.Port = port
  //    uncons.portNumber should equal(ValidPortNumber)
  //  }

}
