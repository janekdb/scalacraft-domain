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

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.OptionValues._

/**
 * Specification for `DomainNameSpec`
 */
class DomainNameSpec extends FlatSpec with Matchers {

  private val MaxLabelLength = 63

  private val MaxOverallLength = 253

  private val MaxSizedLabel = "f" * MaxLabelLength

  behavior of "A DomainName"

  /* Construction from strings */

  it should "not be constructed from a null string" in {
    DomainName.opt(null: String) should be(None)
  }

  it should "not be constructed from an empty string" in {
    DomainName.opt("") should be(None)
  }

  it should "not be constructed from a whitespace string" in {
    DomainName.opt(" ") should be(None)
  }

  it should "not be constructed from a string with any whitespace" in {
    DomainName.opt(" com") should be(None)
    DomainName.opt("c om") should be(None)
    DomainName.opt("com ") should be(None)
  }

  it should "not be constructed from non alphanumeric text" in {
    DomainName.opt("\tcom") should be(None)
    DomainName.opt("c$om") should be(None)
    DomainName.opt("org", "t&me") should be(None)
  }

  it should "not be constructed from surrounding hyphens" in {
    DomainName.opt("-net") should be(None)
    DomainName.opt("net-") should be(None)
  }

  it should "be constructed from a single label" in {
    DomainName.opt("com").value.labels should equal("com" :: Nil)
  }

  it should "be constructed from multiple labels" in {
    DomainName.opt("www", "scalacraft", "com").value.labels should
      equal("www" :: "scalacraft" :: "com" :: Nil)
  }

  it should "be constructed from 127 one character labels" in {
    val labels = List.fill(127)("a")
    DomainName.opt(labels: _*).value.labels should equal(labels)
  }

  it should "not be constructed from 128 one character labels" in {
    val labels = List.fill(128)("a")
    DomainName.opt(labels: _*) should be(None)
  }

  it should "be constructed from a maximum size label" in {
    DomainName.opt(MaxSizedLabel).value.labels should equal(MaxSizedLabel :: Nil)
  }

  it should "not be constructed from an oversized label" in {
    DomainName.opt(MaxSizedLabel + "x") should be(None)
  }

  it should "be constructed when the overall size is at the maximum" in {
    val a = "a" * 49
    assert(a.size <= MaxLabelLength)
    val b = "bcd"
    ((a + ".") * 5 + b) should have length (MaxOverallLength)
    DomainName.opt(a, a, a, a, a, b).value.labels should equal(a :: a :: a :: a :: a :: b :: Nil)
  }

  it should "not be constructed when the overall size would be exceeded" in {
    val a = "a" * 49
    assert(a.size <= MaxLabelLength)
    val b = "bcde"
    ((a + ".") * 5 + b) should have length (MaxOverallLength + 1)
    DomainName.opt(a, a, a, a, a, b) should be(None)
  }

}