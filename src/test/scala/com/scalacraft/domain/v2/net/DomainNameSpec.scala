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

import com.scalacraft.domain.v2.internal.Reflections
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.OptionValues._

import com.scalacraft.domain.v2.net.unconstrained.{DomainName => Other}

/**
 * Specification for `DomainNameSpec`
 */
class DomainNameSpec extends FlatSpec with Matchers {

  private val MaxLabelLength = 63

  private val MaxOverallLength = 253

  private val MaxSizedLabel = "f" * MaxLabelLength

  behavior of "A DomainName"

  /* Construction from strings */

  it should "not be constructed from a null seq" in {
    DomainName.opt(null: _*) should be(None)
  }

  it should "not be constructed from an empty seq" in {
    DomainName.opt() should be(None)
  }

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
    DomainName.opt("-v2") should be(None)
    DomainName.opt("v2-") should be(None)
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
    ((a + ".") * 5 + b) should have length MaxOverallLength
    DomainName.opt(a, a, a, a, a, b).value.labels should equal(a :: a :: a :: a :: a :: b :: Nil)
  }

  it should "not be constructed when the overall size would be exceeded" in {
    val a = "a" * 49
    assert(a.size <= MaxLabelLength)
    val b = "bcde"
    ((a + ".") * 5 + b) should have length (MaxOverallLength + 1)
    DomainName.opt(a, a, a, a, a, b) should be(None)
  }

  /* Construction from a strings with separators */

  it should "be constructed from a multi-label string" in {
    DomainName.opt("example.com").value.labels should equal("example" :: "com" :: Nil)
  }

  it should "be constructed from two multi-label strings" in {
    DomainName.opt("a.b", "example.com").value.labels should equal("a" :: "b" :: "example" :: "com" :: Nil)
  }

  it should "be constructed from two multi-label strings and one other string" in {
    DomainName.opt("a.b", "c", "example.com").value.labels should equal("a" :: "b" :: "c" :: "example" :: "com" :: Nil)
  }

  /* Constructor access */

  it should "not have a public constructor" in {
    val constructors = Reflections.declaredConstructors[DomainName]
    constructors should have size 1
    val con = constructors.head
    con.isPrivate should be(true)
  }

  it should "not allow direct instantiation" in {
    val label1 = "scalacraft"
    val label2 = "org"
    "new DomainName(label1, label2)" shouldNot compile
  }

  /* Pattern Matching */

  it should "be usable in string pattern matching" in {
    def m(x: String): Seq[String] = x match {
      case DomainName(label) => label :: Nil
      case DomainName(label1, label2) => label1 :: label2 :: Nil
      case DomainName(_, _, _) => "unicorn" :: Nil
      case DomainName(label1, label2, label3, label4) => label1 :: label2 :: label3 :: label4 :: Nil
      case _ => Nil
    }
    m("www") should equal("www" :: Nil)
    m("example.com") should equal("example" :: "com" :: Nil)
    m("a.b.c.d") should equal("a" :: "b" :: "c" :: "d" :: Nil)
    m("1.2.3.4.5.6.7") should equal(Nil)
    m("") should be(Nil)
    m("a$") should be(Nil)
    m("a.b-") should be(Nil)
  }

  it should "be usable in string pattern matching with trailing sequences" in {
    def m(x: String): Seq[String] = x match {
      case DomainName(label, labels@_*) => labels
    }
    m("www") should equal(Nil)
    m("www.example.com") should equal("example" :: "com" :: Nil)
  }

  /* Implicit Conversions */

  it should "implicitly convert to a string" in {
    val dnOpt: Option[DomainName] = DomainName.opt("todo", "example", "com")
    val s: String = dnOpt.get
    s should equal("todo.example.com")
  }

  it should "implicitly convert to a seq" in {
    val dnOpt: Option[DomainName] = DomainName.opt("todo", "example", "com")
    val labels: Seq[String] = dnOpt.get
    labels should equal("todo" :: "example" :: "com" :: Nil)
  }

  it should "implicitly convert to a unconstrained domain name" in {
    val dn = DomainName.opt("blog", "scalacraft", "com").get
    val other: Other = dn
    other.labels should equal("blog" :: "scalacraft" :: "com" :: Nil)
  }

  /* Other */

  it should "be case sensitive" in {
    DomainName.opt("WWW") should not equal DomainName.opt("www")
  }
}
