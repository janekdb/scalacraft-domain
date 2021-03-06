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

import com.scalacraft.domain.v2.internal.ex.NullConstructorArgumentException
import com.scalacraft.domain.v2.internal.ex.NullElementException

/**
 * Collection point for the necessary evil of rejecting nulls.
 */
object RejectNullConstructorArgument {

  def apply(param: Any, paramName: String): Unit = {
    // TODO: Remove if statement
    if (param == null)
      throw new NullConstructorArgumentException(paramName)
  }

  def rejectNullElement(param: List[Any], paramName: String): Unit = {
    val firstNullIndex = param.zipWithIndex.collect { case (v, idx) if v == null => idx }.headOption
    firstNullIndex foreach (idx => throw new NullElementException(paramName, idx))
  }
}
