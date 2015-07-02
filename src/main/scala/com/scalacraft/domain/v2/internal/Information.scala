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
   * @param info Conversion function to invoke when `data` contains useful information
   * @tparam T The type the information will be converted to
   * @return Some(None) if there is no information otherwise Some(Some(t)) when `info` can convert the data
   *         to an instance otherwise None which signifies the presence of unconvertible information bearing
   *         data
   */
  def whenSome[T](data: String)(info: String => Option[T]): Option[Option[T]] = {
    data match {
      case Zero() => Some(None)
      case _ => info(data) map (Some(_))
    }
  }

  /**
   * When information exists delegate conversion to the provided function, otherwise convert missing information
   * into Some(`whenZeroInfo`). The initial client was the validated version of OctetPair.
   * @param data The data to convert
   * @param whenZeroInfo When no information present in `data` return Some(whenZeroInfo)
   * @param info Conversion function to invoke when `data` contains useful information
   * @tparam T The type the data is being converted to
   * @return The conversion of `data` to an instance of `T` or not
   */
  def whenSome[T](data: String, whenZeroInfo: T)(info: String => Option[T]): Option[T] = {
    data match {
      case Zero() => Some(whenZeroInfo)
      case _ => info(data)
    }
  }

  /**
   * When all parameters are not null invoke `f` and return the result as a `Some`
   * otherwise when any parameter is null return `None`
   * @param v1 A possibly null value
   * @param v2 A possibly null value
   * @param f The function to call with `v1` and `v2` when both are not null
   * @return A some of the result of applying `f` to the parameters when none are
   *         null
   */
  def whenNotNull[T1, T2, U](v1: T1, v2: T2)(f: (T1, T2) => U): Option[U] =
    for {
      _ <- Option(v1)
      _ <- Option(v2)
    } yield f(v1, v2)

  /**
   * When all parameters are not null invoke `f` and return the result as a `Some`
   * otherwise when any parameter is null return `None`
   * @param v1 A possibly null value
   * @param v2 A possibly null value
   * @param v3 A possibly null value
   * @param v4 A possibly null value
   * @param v5 A possibly null value
   * @param v6 A possibly null value
   * @param v7 A possibly null value
   * @param v8 A possibly null value
   * @param f The function to call with all args when none are null
   * @return A some of the result of applying `f` to the parameters when none are
   *         null
   */
  def whenNotNull
  [T1, T2, T3, T4, T5, T6, T7, T8, U]
  (v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8)
  (f: (T1, T2, T3, T4, T5, T6, T7, T8) => U): Option[U] =
    for {
      _ <- Option(v1)
      _ <- Option(v2)
      _ <- Option(v3)
      _ <- Option(v4)
      _ <- Option(v5)
      _ <- Option(v6)
      _ <- Option(v7)
      _ <- Option(v8)
    } yield f(v1, v2, v3, v4, v5, v6, v7, v8)

}
