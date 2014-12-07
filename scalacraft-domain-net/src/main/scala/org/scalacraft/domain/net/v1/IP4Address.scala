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

import scala.util.control.Exception._

/**
 * `IP4Address`
 */
case class IP4Address(byte1: Int, byte2: Int, byte3: Int, byte4: Int) {
  private def tuple = (byte1, byte2, byte3, byte4)
}

object IP4Address {

  /**
   * Provide a string representation for an instance of this class
   * @param ip4Address The instance to use.
   * @return A dotted quad. For example "192.168.0.23"
   */
  implicit def `to-String`(ip4Address: IP4Address): String =
    ip4Address.tuple.productIterator.mkString(".")

  private val ValidRange = Range(0, 255 + 1)

  private implicit def ix(i: Int) = new {
    def maybe: Option[Int] = Some(i) filter (ValidRange contains)
  }

  private val DottedQuadPat = {
    val digits = "([\\d]{1,3})"
    val dot = "\\."
    ((digits + dot) * 3 + digits).r
  }

  private implicit class sx(val s: String) {
    def optInt = catching(classOf[NumberFormatException]) opt s.toInt
  }

  def opt(byte1: Int, byte2: Int, byte3: Int, byte4: Int): Option[IP4Address] =
    for {
      b1 <- byte1.maybe
      b2 <- byte2.maybe
      b3 <- byte3.maybe
      b4 <- byte4.maybe
    }
    yield IP4Address(b1, b2, b3, b4)

  def opt(quad: String): Option[IP4Address] = quad match {
    case DottedQuadPat(s1, s2, s3, s4) => {
      for {
        b1 <- s1.optInt
        b2 <- s2.optInt
        b3 <- s3.optInt
        b4 <- s4.optInt
        r <- opt(b1, b2, b3, b4)
      } yield r
    }
    case _ => None
  }

  def unapply(x: String): Option[(Int, Int, Int, Int)] =
    opt(x) map (_.tuple)

}