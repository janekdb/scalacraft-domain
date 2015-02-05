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

/**
 * A `CountryCodeA3` represents an ISO 3166 alpha-2 country code.
 *
 * Details of the ISO 3166-1 alpha 3 codes can be found here: [[http://en.wikipedia.org/wiki/ISO_3166-1_alpha-3]]
 *
 * === Pattern Matching ===
 *
 * Pattern matching is supported as the following example demonstrates,
 * {{{
 * "JAM" match {
 * case CountryCodeA3(code) => code  // "JAM"
 * case _ => None
 * }
 * }}}
 * Invalid country codes will not be matched.
 * {{{
 * "JA" match {
 * case CountryCodeA3(code) => code
 * case _ => None  // None
 * }
 * }}}
 *
 * === Implicit Conversions ===
 *
 * Implicit conversions exist which allow an instance of `CountryCodeA3` to be used when a `String`
 * is required.
 *
 * {{{
 *   def logCode(code: String) = println(s"code: $code")
 *
 *   val cc = CountryCodeA3.opt("TKL").get
 *   logCode // "code: TKL"
 * }}}
 *
 * A conversion to the unconstrained version of this class is also available.
 *
 * TODO: Documentation
 */
case class CountryCodeA3 private(countryCode: String)

/**
 * TODO: Documentation
 */
object CountryCodeA3 {

  implicit def `to-String`(countryCode: CountryCodeA3): String = countryCode.countryCode

  /**
   * Match three upper case letters
   **/
  private val CodePat = "^[A-Z]{3}$".r

  /**
   * TODO: Documentation
   */
  def opt(countryCode: String): Option[CountryCodeA3] =
    CodePat findFirstIn countryCode map apply

  def unapply(x: String): Option[String] =
    CodePat findFirstIn x
}

// TODO: Refactor the objects for A2 and A3 into a trait with an abstract type.