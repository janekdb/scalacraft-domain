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

import com.scalacraft.domain.v2.country.{CountryCodeA2 => ConstrainedCountryCodeA2}

/**
 * A `CountryCodeA2` represents an ISO 3166 alpha-2 country code.
 *
 * This class does not constrain the value of the code in anyway.
 *
 * Details of the ISO 3166-1 alpha 2 codes can be found here: [[http://en.wikipedia.org/wiki/ISO_3166-1_alpha-2]]
 *
 * === Pattern Matching ===
 *
 * Pattern matching is supported as illustrated by the following examples,
 * {{{
 * "SC" match {
 * case CountryCodeA2(code) => code  // "SC"
 * case _ => None
 * }
 * }}}
 * Invalid country codes will be matched.
 * {{{
 * "GBR" match {
 * case CountryCodeA2(code) => code // "GBR"
 * case _ => None
 * }
 * }}}
 *
 * === Implicit Conversions ===
 *
 * An implicit conversion is provided which allows an instance of `CountryCodeA2` to be used when a `String`
 * is required.
 *
 * {{{
 *   def logCode(code: String) = println(s"code: $code")
 *
 *   val cc = CountryCodeA2("TV")
 *   logCode(cc) // "code: TV"
 * }}}
 *
 * An implicit conversion to the constrained version of this class is also available.
 *
 * @param countryCode A value which can be a valid or invalid alpha 2 country code
 */
case class CountryCodeA2(countryCode: String)

/**
 * This object provides a set of operations needed to create and convert instances of [[CountryCodeA2]]
 */
object CountryCodeA2 {

  /**
   * Provide direct access to the country code.
   *
   * @param countryCode The instance to use.
   * @return The country code. For example "PL"
   */
  implicit def `to-String`(countryCode: CountryCodeA2): String = countryCode.countryCode

  implicit def `to-Option[CountryCodeA2]`(countryCode: CountryCodeA2): Option[ConstrainedCountryCodeA2] =
    ConstrainedCountryCodeA2.opt(countryCode.countryCode)

  def unapply(x: String): Option[String] = Some(x)

}