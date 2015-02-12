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

import com.scalacraft.domain.v2.internal.Reflections
import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.OptionValues._

/**
 * Specification for `CountryCodeNumeric`
 */
class CountryCodeNumericSpec extends FlatSpec with Matchers {

  private val ValidCountryCode = "801"

  private val ShortCountryCode = "44"

  private val LongCountryCode = ValidCountryCode * 2

  private val InvalidCharsCountryCode = "A83"

  private val NegativeCountryCode = "-801"

  behavior of "A CountryCodeNumeric"

  /* Construction from a string */

  it should "be constructed from a valid country code" in {
    val ccOpt: Option[CountryCodeNumeric] = CountryCodeNumeric.opt(ValidCountryCode)
    ccOpt.value.countryCode should equal(ValidCountryCode)
  }

  it should "not be constructed from a short country code" in {
    CountryCodeNumeric.opt(ShortCountryCode) should be(None)
  }

  it should "not be constructed from a long country code" in {
    CountryCodeNumeric.opt(LongCountryCode) should be(None)
  }

  it should "not be constructed from a country code with invalid characters" in {
    CountryCodeNumeric.opt(InvalidCharsCountryCode) should be(None)
  }

  it should "not be constructed from a negative country code" in {
    CountryCodeNumeric.opt(NegativeCountryCode) should be(None)
  }

  /* Constructor access */

  it should "not have a public constructor" in {
    val constructors = Reflections.declaredConstructors[CountryCodeNumeric]
    constructors should have size (1)
    val con = constructors.head
    con.isPrivate should be(true)
  }

  /* Pattern Matching */

  it should "be usable in string pattern matching" in {
    def m(x: String) = x match {
      case CountryCodeNumeric(cc) => cc
      case _ => None
    }
    m(ValidCountryCode) should equal(ValidCountryCode)
    m(InvalidCharsCountryCode) should be(None)
  }

  /* Implicit Conversions */

  it should "implicitly convert to a string" in {
    val Some(cc) = CountryCodeNumeric.opt(ValidCountryCode)
    val s: String = cc
    s should equal(ValidCountryCode)
  }

  it should "implicitly convert to an unconstrained CountryCodeNumeric" in {
    val Some(cc) = CountryCodeNumeric.opt(ValidCountryCode)
    val uncons: unconstrained.CountryCodeNumeric = cc
    uncons.countryCode should equal(ValidCountryCode)
  }

}
