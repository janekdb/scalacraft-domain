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
package org.scalacraft.domain.v2.net.unconstrained

import org.scalacraft.domain.v2.net.{DomainName => ConstrainedDomainName}

/**
 * A `DomainName` represents a name in the domain name system.
 *
 * This class does not constrain the value of the domain name in anyway.
 *
 * Unlike the example in Programming in Scala/2e the elements of the domain name are stored in the same order
 * as used, i.e. as `www.scalacraft.com`, not reversed.
 *
 * This implementation is case sensitive which is at variance with the Wikipedia specification. If a convincing
 * use case for case insensitivity arises this could be reconsidered. Expressed in code we have,
 * {{{
 *   DomainName("WWW") != DomainName("www")
 * }}}
 *
 * === Pattern Matching ===
 *
 * Pattern matching is supported as the following example demonstrates,
 * {{{
 * "example.com" match {
 * case DomainName(label1, label2) => label1 :: label2 :: Nil  // List("example", "com")
 * case _ => None
 * }
 * }}}
 * Domain names with invalid labels will also be matched because this class is unconstrained,
 * {{{
 * "a.b-" match {
 * case DomainName(label1, label2) => label1 :: label2 :: Nil  // List("a", "b-")
 * case _ => None
 * }
 * }}}
 * For now there is no way to match an arbitrary number of domain name labels.
 *
 * === Implicit Conversions ===
 *
 * Implicit conversions exist which allow an instance of `DomainName` to be used when either a `String`
 * or `Seq[String]` is required.
 *
 * {{{
 *   def countElements(seq: Seq[String]) = seq.length
 *
 *   val dn = DomainName("www", "example", "com")
 *   val elemNo = countElements(dn) // 3
 * }}}
 *
 * A conversion to an option of the constrained version of this class is also available.
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