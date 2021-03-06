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

import scala.reflect.runtime.universe._

/**
 * `Reflections`
 */
object Reflections {

  /**
   * Extract the declared constructors for the given type.
   * @tparam T The type to reflect on
   * @return A list of declared constructors
   */
  def declaredConstructors[T: TypeTag]: Iterable[MethodSymbol] =
    typeOf[T].
      decls.
      filter(_.isMethod).
      map(_.asMethod).
      filter(_.isConstructor)

}
