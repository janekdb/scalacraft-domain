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
package org.scalacraft.domain.net.v1

/**
 * A `DomainName` represents a name in the domain name system.
 *
 * The syntactic constraints on a domain name are taken from [[http://en.wikipedia.org/wiki/Domain_name Wikipedia: Domain Name]],
 * excerpted here for ease of reference. Unlike the example in Programming in Scala/2e the elements of the
 * domain name are stored in the same order as use, i.e. as `www.scalacraft.com`, not reversed. This
 * implementation is case sensitive which is at variance with the Wikipedia specification. Since there is
 * no available use case for case insensitivity this requirement is ignored. Expressed as truth in code we have,
 * {{{
 *   DomainName.opt("WWW") != DomainName.opt("www")
 * }}}
 *
 * '''Wikipedia'''
 *
 * Domain names may be formed from the set of alphanumeric ASCII characters (a-z, A-Z, 0-9), but characters
 * are case-insensitive. In addition the hyphen is permitted if it is surrounded by characters, digits or
 * hyphens, although it is not to start or end a label.
 *
 * The hierarchy of domains descends from the right to the left label in the name; each label to the left
 * specifies a subdivision, or subdomain of the domain to the right. For example: the label example specifies
 * a node example.com as a subdomain of the com domain, and www is a label to create www.example.com,
 * a subdomain of example.com. This tree of labels may consist of 127 levels. Each label may contain
 * from 1 to 63 octets. The empty label is reserved for the root node. The full domain name may not exceed a
 * total length of 253 ASCII characters in its textual representation. In practice, some domain registries
 * may have shorter limits.
 */
case class DomainName private(labels: String*)

object DomainName {

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

  private def labelMatchesPattern(label: String) = LabelPat findFirstIn (label) isDefined

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
      labels <- Option(labels) filter (_.forall(_ != null)) map (split)
      if labelCountInRange(labels)
      if labels forall (labelSizeInRange)
      if labels forall (labelMatchesPattern)
      if overallLengthInRange(labels)
    }
    yield DomainName(labels: _*)
  }

  def unapplySeq(x: String): Option[Seq[String]] = opt(x) map (_.labels)
}