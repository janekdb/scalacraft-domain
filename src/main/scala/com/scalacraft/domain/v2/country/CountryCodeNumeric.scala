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

import com.scalacraft.domain.v2.country.unconstrained.{CountryCodeNumeric => UnconstrainedCountryCodeNumeric}
import com.scalacraft.domain.v2.internal.SingleParam

/**
 * A `CountryCodeNumeric` represents an ISO 3166 numeric country code.
 *
 * Details of the ISO 3166-1 numeric codes can be found here: [[http://en.wikipedia.org/wiki/ISO_3166-1_numeric]]
 *
 * === Pattern Matching ===
 *
 * Pattern matching is supported as illustrated by the following examples,
 * {{{
 * "343" match {
 * case CountryCodeNumeric(code) => code  // "343"
 * case _ => None
 * }
 * }}}
 * Invalid country codes will not be matched.
 * {{{
 * "44" match {
 * case CountryCodeNumeric(code) => code
 * case _ => None  // None
 * }
 * }}}
 *
 * === Implicit Conversions ===
 *
 * An implicit conversion is provided which allows an instance of `CountryCodeNumeric` to be used when a `String`
 * is required.
 *
 * {{{
 *   def logCode(code: String) = println(s"code: $code")
 *
 *   val cc = CountryCodeNumeric.opt("495").get
 *   logCode(cc) // "code: 495"
 * }}}
 *
 * An implicit conversion to the unconstrained version of this class is also available.
 *
 * @param countryCode A valid numeric country code
 */
case class CountryCodeNumeric private(countryCode: String)

/**
 * This object provides a set of operations needed to create and convert instances of [[CountryCodeNumeric]]
 */
object CountryCodeNumeric extends SingleParam[CountryCodeNumeric] {

  /**
   * Provide direct access to the country code.
   *
   * @param countryCode The instance to use.
   * @return The country code. For example "712"
   */
  @deprecated(since = "2.1.0")
  implicit def `to-String`(countryCode: CountryCodeNumeric): String = countryCode.countryCode

  implicit def `to-[CountryCodeNumeric]`(countryCode: CountryCodeNumeric): UnconstrainedCountryCodeNumeric =
    UnconstrainedCountryCodeNumeric(countryCode.countryCode)

  /**
   * Match three digits
   **/
  protected val ValuePat = "^[0-9]{3}$".r

  /**
   * @return A non-empty option when `countryCode` is a syntactically valid country code.
   */
  override def opt(countryCode: String): Option[CountryCodeNumeric] = super.opt(countryCode)

}