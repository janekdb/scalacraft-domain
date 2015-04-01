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

/**
 * This object is a collection point for transformations related to
 * information bearing data.
 */
object Information {

  object Zero {
    def unapply(s: String): Boolean = s == null || s.trim.isEmpty
  }

  /**
   * When information exists delegate conversion to the provided function, otherwise convert missing information
   * into Some(None). This supports `Rule 3 - Additional Freedom`
   * @param data The information source
   * @param info Conversion code to invoke if `data` contains any useful information
   * @tparam T The type the information will be converted to
   * @return Some(None) if there is no information otherwise Some(Some(t)) when `info` can convert the data
   *         to an instance otherwise None which signifies the presence of unconvertible information bearing
   *         data
   */
  def whenSome[T](data: String)(info: String => Option[Option[T]]): Option[Option[T]] = {
    data match {
      case Zero() => Some(None)
      case _ => info(data)
    }
  }
}
