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
    val result = Information.whenSome("5") { case x => Some(x.toInt)}
    result should equal(Some(Some(5)))
  }

  it should "passthrough conversion to None" in {
    val result = Information.whenSome("five") { case x => None}
    result should be(None)
  }

  private val X = "X"

  it should "convert null to X when X is specified as the zero info conversion" in {
    val result = Information.whenSome(null, X) { case x => throw new RuntimeException}
    result should equal(Some(X))
  }

  it should "convert a whitespace string to X when X is specified as the zero info conversion" in {
    val result = Information.whenSome(" " * 4, X) { case x => throw new RuntimeException}
    result should equal(Some(X))
  }

  it should "passthrough conversion of A to Some(AA) when X is specified as the zero info conversion" in {
    val result = Information.whenSome("A", X) { case x => Some(x * 2)}
    result should equal(Some("AA"))
  }

  it should "passthrough conversion to None when X is specified as the zero info conversion" in {
    val result = Information.whenSome("A", X) { case x => None}
    result should be(None)
  }

  behavior of "Information.whenNotNull"

  it should "call f only when both arguments are not null" in {
    val A = "a"
    val B = "b"
    def add(i: String, j: String): String = i + j
    Information.whenNotNull(null, null)(add) should be(None)
    Information.whenNotNull(A, null)(add) should be(None)
    Information.whenNotNull(null, B)(add) should be(None)
    Information.whenNotNull(A, B)(add) should be(Some(A + B))
  }
}
