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

import scala.util.matching.Regex

/**
 * This trait is a collection point for methods applicable to single parameter domain types.
 */
trait SingleParam[T] {

  /**
   * The pattern a candidate value must match to be considered valid for the type `T`
   */
  protected def ValuePat: Regex

  def apply(value: String): T

  /**
   * @return A non-empty option when `candidateValue` is a syntactically valid value for the type `T`.
   */
  def opt(candidateValue: String): Option[T] =
    ValuePat findFirstIn candidateValue map apply

  /**
   * Allow pattern matching via extraction.
   */
  def unapply(candidateValue: String): Option[String] =
    ValuePat findFirstIn candidateValue

}
