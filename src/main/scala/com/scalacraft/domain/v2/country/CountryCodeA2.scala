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
 * A `CountryCodeA2` represents an ISO 3166 alpha-2 country code.
 *
 * Reference: http://en.wikipedia.org/wiki/ISO_3166-1_alpha-2
 * 
 * TODO: Documentation
 */
// TODO: Add test for no public constructor
case class CountryCodeA2(countryCode: String)

/**
 * TODO: Documentation
 */
object CountryCodeA2 {

  implicit def `to-String`(countryCode: CountryCodeA2): String = countryCode.countryCode

  /**
   * Match two upper case letters
   **/
  private val CodePat = "^[A-Z]{2}$".r

  /**
   * TODO: Documentation
   */
  def opt(countryCode: String): Option[CountryCodeA2] =
    CodePat findFirstIn countryCode map apply

  def unapply(x: String): Option[String] =
    CodePat findFirstIn x
}
