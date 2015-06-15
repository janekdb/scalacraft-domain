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
package com.scalacraft.domain.v2.net

import com.scalacraft.domain.v2.net.unconstrained.{DomainName => UnconstrainedDomainName}

/**
 * A `DomainName` represents a name in the domain name system.
 *
 * Unlike the example in Programming in Scala/2e the elements of the domain name are stored in the same order
 * as used, i.e. as `www.scalacraft.com`, not reversed.
 *
 * This implementation is case sensitive which is at variance with the Wikipedia specification. If a convincing
 * use case for case insensitivity arises this could be reconsidered. Expressed in code we have,
 * {{{
 *   DomainName.opt("WWW") != DomainName.opt("www")
 * }}}
 *
 * The v2 documentation provides details of the syntactic constraints placed on domain names.
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
 * Invalid domain names will not be matched.
 * {{{
 * "a.b-" match {
 * case DomainName(label1, label2) => label1 :: label2 :: Nil
 * case _ => None  // None
 * }
 * }}}
 * To match an arbitrary number of domain name labels use a pattern sequence,
 * {{{
 * "www.example.com" match {
 * case DomainName(label, labels@_*) => labels  // "example"::"com"::Nil
 * }
 * }}}
 *
 * === Implicit Conversions ===
 *
 * Implicit conversions exist which allow an instance of `DomainName` to be used when either a `String`
 * or `Seq[String]` is required.
 *
 * {{{
 *   def countElements(seq: Seq[String]) = seq.length
 *
 *   val dn = DomainName.opt("www", "example", "com").get
 *   val elemNo = countElements(dn) // 3
 * }}}
 *
 * A conversion to the unconstrained version of this class is also available.
 */
case class DomainName private(labels: String*)

object DomainName {

  /**
   * Provide a string representation for an instance of this class.
   * @param domainName The instance to use
   * @return A domain name as a string
   */
  @deprecated(since = "2.1.0")
  implicit def `to-String`(domainName: DomainName): String = domainName.labels mkString LabelSeparator.toString

  /**
   * Provide direct access to the labels of a domain name.
   * @param domainName The instance to use
   * @return The domain name labels
   */
  @deprecated(since = "2.1.0")
  implicit def `to-Seq[String]`(domainName: DomainName): Seq[String] = domainName.labels

  implicit def `to-DomainName`(domainName: DomainName): UnconstrainedDomainName =
    UnconstrainedDomainName(domainName.labels: _*)

  private val LabelSeparator = '.'

  private val MaxOverallLength = 253

  /**
   * The restriction on the maximum overall length implies this restriction on the
   * maximum label count. Even so the label count is explicitly tested to make clear
   * the nature of the restriction.
   */
  private val LabelCountRange = 1 to 127

  private def labelCountInRange(labels: Seq[String]) = LabelCountRange contains labels.size

  /**
   * A pattern that does not match leading or trailing hyphens
   * See: http://www.regular-expressions.info/lookaround.html
   **/
  private val LabelPat = "^(?!-)[-a-zA-Z0-9]++(?<!-)$" r

  private def labelMatchesPattern(label: String) = LabelPat findFirstIn label isDefined

  private val LabelRange = 1 to 63

  private def labelSizeInRange(label: String) = LabelRange contains label.size

  /** "www.scalacraft.com".length = "wwwscalacraftcom".length + 2 */
  private def overallLengthInRange(labels: Seq[String]) =
    labels.map(_.length).sum + labels.size - 1 <= MaxOverallLength

  private def split(labels: Seq[String]): Seq[String] = labels map (_.split(LabelSeparator)) flatten

  /**
   * If `labels` is a collection of valid labels or a single string with embedded label separators then
   * return an instance other return None.
   * Examples of valid labels
   * - "www", "scalacraft", "com"
   * - "www.scalacraft", "com"
   * - "www.scalacraft.com"
   * Examples of invalid labels
   * - "www.", "$calacraft", "com-"
   * - ""
   * @param labels A sequence of domain name parts or a single string with embedded periods.
   * @return A valid instance of `DomainName` or `None` if any constraint was violated.
   */
  def opt(labels: String*): Option[DomainName] = {

    /* Option(seq) converts a null to a None but does not inspect the elements so filter is used. */
    for {
      labels <- Option(labels) filter (_.forall(_ != null)) map split
      if labelCountInRange(labels)
      if labels forall labelSizeInRange
      if labels forall labelMatchesPattern
      if overallLengthInRange(labels)
    }
    yield DomainName(labels: _*)
  }

  def unapplySeq(x: String): Option[Seq[String]] = opt(x) map (_.labels)
}