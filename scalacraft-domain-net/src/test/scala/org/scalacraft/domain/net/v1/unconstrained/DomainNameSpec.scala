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

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import org.scalatest.OptionValues._

import org.scalacraft.domain.net.v1.{DomainName => Other}

/**
 * Specification for an unconstrained `DomainNameSpec`
 */
class DomainNameSpec extends FlatSpec with Matchers {

  behavior of "An unconstrained DomainName"

  /* Pattern Matching */

  it should "be usable in string pattern matching" in {
    def m(x: String): Seq[String] = x match {
      case DomainName(label) => label :: Nil
      case DomainName(label1, label2) => label1 :: label2 :: Nil
      case DomainName(_, _, _) => "unicorn" :: Nil
      case DomainName(label1, label2, label3, label4) => label1 :: label2 :: label3 :: label4 :: Nil
      case _ => Nil
    }
    m("") should equal(""::Nil)
    m("www") should equal("www" :: Nil)
    m("example.com") should equal("example" :: "com" :: Nil)
    m("a.b.c.d") should equal("a" :: "b" :: "c" :: "d" :: Nil)
    m("a$.-b. c.d") should equal("a$" :: "-b" :: " c" :: "d" :: Nil)
    m("") should be("" :: Nil)
    m("a$") should be("a$" :: Nil)
    m("a.b-") should be("a" :: "b-" :: Nil)
  }

  /* Implicit Conversions */

  it should "implicitly convert to a string" in {
    val dn: DomainName = DomainName("todo", "example", "com")
    val s: String = dn
    s should equal("todo.example.com")
  }

  it should "implicitly convert to a seq" in {
    val dn: DomainName = DomainName("todo", "example", "com")
    val labels: Seq[String] = dn
    labels should equal("todo" :: "example" :: "com" :: Nil)
  }

  it should "implicitly convert to a constrained domain name when valid" in {
    val dn = DomainName("blog", "scalacraft", "com")
    val otherOpt: Option[Other] = dn
    otherOpt.value.labels should equal("blog" :: "scalacraft" :: "com" :: Nil)
  }

  it should "implicitly convert to None when domain name is invalid" in {
    val dn = DomainName("*", "scalacraft", "com")
    val otherOpt: Option[Other] = dn
    otherOpt should be(None)
  }
}
