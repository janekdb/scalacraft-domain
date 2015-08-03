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
package com.scalacraft.domain.v2.net.unconstrained

import com.scalacraft.domain.v2.internal.ex.NullConstructorArgumentException
import com.scalacraft.domain.v2.internal.ex.NullElementException
import com.scalacraft.domain.v2.binary.unconstrained.OctetPair

import org.scalatest.{Ignore, FlatSpec, Matchers}
import org.scalatest.OptionValues._

/**
 * Specification for an unconstrained `IP6Address`
 */
class IP6AddressSpec extends FlatSpec with Matchers {

  behavior of "An unconstrained IP6Address"

  /* Constructor args */

  it should "reject null constructor args" in {

    the[NullConstructorArgumentException] thrownBy {
      new IP6Address(null)
    } should have('paramName("octetPairs"))
  }

  it should "reject constructor args that embed nulls" in {
    val op1 = new OctetPair(None, None)
    val op2: OctetPair = null
    val invalidOctetPairs = op1 :: op2 :: Nil

    the[NullElementException] thrownBy {
      new IP6Address(invalidOctetPairs)
    } should have(
      'paramName("octetPairs"),
      'index(1),
      'message("octetPairs(1)")
    )
  }

  /* Pattern Matching */

  //  IP6Address has less enforcement in it's constructor than IP4Address
  //  and the extractor should follow this. As an API law for IP4 we have
  //  For each IP4Address the string repr should be matched to. Clearly the
  //  reverse relationship holds because the match results in an instance of
  //  IP6Address. If IP6Address can be constructed from null, empty and any
  //  non-empty string then the extractor should do the same.

  it should "be usable in string pattern matching" in {
    //    def m(x: String) = x match {
    //      case IP6Address(repr) => repr
    //      case _ => None
    //    }
    //    m("") should be ("")
    //    m("xx") should be("xx")
    //    m("55.66.77") should be("55.66.77")
    //    m("::") should be ("::")
    //    m("0000:0000:0000:0000:0000:0000:0000:0000") should be ("0000:0000:0000:0000:0000:0000:0000:0000")
    //    m("fe80::2000:0aff:fea7:0f7c") should be ("fe80::2000:0aff:fea7:0f7c")
    //    m("fe80:0000:0000:0000:2000:0aff:fea7:0f7c") should be ("fe80:0000:0000:0000:2000:0aff:fea7:0f7c")
  }

}
