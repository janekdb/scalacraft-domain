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
package com.scalacraft.domain.v2.binary

import com.scalacraft.domain.v2.internal.NumericConversions.FromString

/**
 * TODO: Documentation
 * `Octet`
 */
case class Octet private(octet: Int)

// TODO: Add trait to allow merge with Port following CountryCode regex example
object Octet {

  implicit def `to-Int`(octet: Octet): Int = octet.octet

  /**
   * @example Given Octet(47) this will return `2f`
   * @param octet The instance to extract a value from
   * @return A string representation of the octet as two hex character without any prefix
   */
  implicit def `to-String`(octet: Octet): String = octet.octet.formatted("%02x")

  //  implicit def `to-Octet`(octet: Octet): unconstrained.Octet = unconstrained.Octet(???)

  private val HexPat = "^([0-9a-f]{1,2})$" r

  private val inRange = Range(0, 255 + 1).contains _

  /**
   * @param candidateValue A number
   * @return A some of Octet if the number is in the range [0,255] otherwise `None`
   */
  def opt(candidateValue: Int): Option[Octet] =
    Some(candidateValue) filter inRange map apply

  /**
   * @param candidateValue A number represented as hex
   * @return A some of Octet if the string is one or two hex characters otherwise `None`
   */
  def opt(candidateValue: String): Option[Octet] =
    Option(candidateValue).map(_.toLowerCase).flatMap {
      case HexPat(digits) => digits.optHexInt map apply
      case _ => None
    }

  def unapply(x: Int): Option[Int] =
    Some(x) filter inRange

  def unapply(x: String): Option[Int] =
    opt(x) map (_.octet)

}