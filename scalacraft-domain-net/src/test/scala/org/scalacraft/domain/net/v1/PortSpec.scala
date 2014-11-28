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

/**
 * Specification for `Port`
 */
class PortSpec extends FlatSpec with Matchers {

  private val ValidPortNumber = 5511

  private val InvalidPortNumber = -129

  "A Port" should "be constructed from a valid port number" in {
    val portOpt: Option[Port] = Port.opt(ValidPortNumber)
    portOpt should be('defined)
    portOpt.get should have(
      'portNumber(ValidPortNumber)
    )
  }

  it should "not be constructed from an invalid port number" in {
    Port.opt(InvalidPortNumber) should be(None)
  }

  it should "be usable in pattern matching" in {
    Port.opt(5) should matchPattern { case Some(Port(5)) =>}
  }

  it should "not have one-off errors" in {
    Port.opt(-1) should be(None)
    Port.opt(0) should matchPattern { case Some(Port(0)) =>}
    Port.opt(65535) should matchPattern { case Some(Port(65535)) =>}
    Port.opt(65535 + 1) should be(None)
  }


  it should "be constructed from a valid port number string" in {
    val portOpt: Option[Port] = Port.opt(ValidPortNumber.toString)
    portOpt should be('defined)
    portOpt.get should have(
      'portNumber(ValidPortNumber)
    )
  }

  it should "not be constructed from an invalid port number string" in {
    Port.opt(InvalidPortNumber.toString) should be(None)
  }

  it should "not be constructed from a null string" in {
    Port.opt(null: String) should be(None)
  }

  it should "not be constructed from an empty string" in {
    Port.opt("") should be(None)
  }

  it should "not be constructed from a non-numeric string" in {
    Port.opt("3rd") should be(None)
  }

  it should "not be constructed from an out of range positive integer string" in {
    val n = (Int.MaxValue: Long) + 1
    Port.opt(n.toString) should be(None)
  }

  it should "not be constructed from an out of range negative integer string" in {
    val n = (Int.MinValue: Long) - 1
    Port.opt(n.toString) should be(None)
  }
}
