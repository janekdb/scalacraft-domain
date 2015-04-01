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
package com.scalacraft.domain.v2.internal

import org.scalatest.{FlatSpec, Matchers}

/**
 * Specification for `Information` object
 */
class InformationSpec extends FlatSpec with Matchers {

  behavior of "An Information object"

  it should "convert null to Some(None)" in {
    val result = Information.whenSome(null) { case x => throw new RuntimeException}
    result should equal(Some(None))
  }

  it should "convert a whitespace string to Some(None)" in {
    val result = Information.whenSome(" " * 4) { case x => throw new RuntimeException}
    result should equal(Some(None))
  }

  it should "passthrough conversion of a number string to Some(Some(n))" in {
    val result = Information.whenSome("5") { case x => Some(Some(x.toInt))}
    result should equal(Some(Some(5)))
  }

  it should "passthrough conversion to None" in {
    val result = Information.whenSome("five") { case x => None}
    result should be(None)
  }
}
