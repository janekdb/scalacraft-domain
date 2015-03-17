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
package com.scalacraft.domain.v2.internal

import scala.util.control.Exception._

/**
 * `NumericConversions`
 *
 * This object contains implicit views which can be imported to add numeric conversion
 * methods.
 */
object NumericConversions {

  /**
   * An implicit view which adds numeric conversions to string instances.
   *
   * @param s The string to augment
   */
  implicit class FromString(val s: String) {
    /**
     * @return The integer value of the string or None if it was not possible
     *         to convert the string to an integer.
     */
    def optInt = catching(classOf[NumberFormatException]) opt s.toInt

    /**
     * @return The integer value of the hex string or None if it was not possible
     *         to convert the string to an integer.
     */
    def optHexInt = catching(classOf[NumberFormatException]) opt Integer.parseInt(s, 0x10)
  }

}
