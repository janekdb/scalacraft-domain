/*
   Copyright 2014 Janek Bogucki

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
package org.scalacraft.domain.net.v1.unconstrained

import org.scalacraft.domain.net.v1.{DomainName => ConstrainedDomainName}

/**
 * `DomainName`
 * TODO: Documentation
 */
case class DomainName(labels: String*)

object DomainName {

  /**
   * Provide a string representation for an instance of this class.
   * @param domainName The instance to use
   * @return A domain name as a string
   */
  implicit def `to-String`(domainName: DomainName): String = domainName.labels mkString LabelSeparator.toString

  /**
   * Provide direct access to the labels of a domain name.
   * @param domainName The instance to use
   * @return The domain name labels
   */
  implicit def `to-Seq[String]`(domainName: DomainName): Seq[String] = domainName.labels

  implicit def `to-Option[DomainName]`(domainName: DomainName): Option[ConstrainedDomainName] =
    ConstrainedDomainName.opt(domainName.labels: _*)

  private val LabelSeparator = '.'

  def unapplySeq(x: String): Option[Seq[String]] = Some(x.split(LabelSeparator).toSeq)
}