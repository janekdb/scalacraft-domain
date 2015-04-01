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
package com.scalacraft.domain.v2.binary.unconstrained

import com.scalacraft.domain.v2.binary.{Octet => ConstrainedOctet}
import com.scalacraft.domain.v2.internal.Information
import com.scalacraft.domain.v2.internal.NumericConversions.FromString
import com.scalacraft.domain.v2.internal.RejectNullConstructorArgument

/**
 * TODO: Documentation
 * `Octet`
 */
case class Octet(octet: Option[Int]) {
  RejectNullConstructorArgument(octet, "octet")
}

object Octet {

  implicit def `to-Option-Int`(octet: Octet): Option[Int] = octet.octet

  /**
   * @example Given Octet(47) this will return `2f`
   * @param octet The instance to extract a value from
   * @return A string representation of the octet as at least two hex characters with no `0x` prefix.
   * @example `f00f` or `0b`
   */
  implicit def `to-Option-String`(octet: Octet): Option[String] = octet.octet.map(_.formatted("%02x"))

  implicit def `to-Option[Octet]`(octet: Octet): Option[ConstrainedOctet] =
    octet.octet.flatMap(ConstrainedOctet.opt)

  def unapply(x: Int): Option[Option[Int]] = Some(Some(x))

  def unapply(x: String): Option[Option[Int]] = Information.whenSome(x)(_.optHexInt)
}