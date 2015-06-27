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
 * Specification for `ReflectionsSpec`
 */
class ReflectionsSpec extends FlatSpec with Matchers {

  private case class A(value: Any)

  private case class B private(value: Any)

  private class C(value: Any) {
    def this() = this(Nil)
  }

  behavior of "A Reflections"

  it should "extract a non-private case class constructor" in {
    val cons = Reflections.declaredConstructors[A]
    cons should have size 1
    val con = cons.head
    con.name.toString should equal("<init>")
    con.isPrivate should be(false)
  }

  it should "extract a private case class constructor" in {
    val cons = Reflections.declaredConstructors[B]
    cons should have size 1
    val con = cons.head
    con.name.toString should equal("<init>")
    con shouldBe 'private
  }

  it should "extract multiple constructors" in {
    val cons = Reflections.declaredConstructors[C]
    cons should have size 2
    cons map (_.name.toString) should equal("<init>" :: "<init>" :: Nil)
  }

}
