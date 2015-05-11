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

import org.scalatest.{Matchers, FlatSpec}
import org.scalatest.OptionValues._

import NumericConversions._

/**
 * `NumericConversionsSpec`
 */
class NumericConversionsSpec extends FlatSpec with Matchers {

  behavior of "The implicits on NumericConversions"

  /* Conversions from String */

  it should "convert valid integer strings to ints" in {
    "0".optInt.value should equal(0)
    "100".optInt.value should equal(100)
    "-90102".optInt.value should equal(-90102)
  }

  it should "not convert invalid integer strings to ints" in {
    (null: String).optInt should be(None)
    "".optInt should be(None)
    "NaN".optInt should be(None)
    "0xF".optInt should be(None)
    "0.1".optInt should be(None)
    "IX".optInt should be(None)
    "full fathom five thy father lies".optInt should be(None)
  }

  it should "convert valid hex integer strings to ints" in {
    "0".optHexInt.value should equal(0)
    "100".optHexInt.value should equal(256)
    "-ff".optHexInt.value should equal(-255)
  }

  it should "not convert invalid hex integer strings to ints" in {
    (null: String).optHexInt should be(None)
    "".optHexInt should be(None)
    "NaN".optHexInt should be(None)
    "0xG".optHexInt should be(None)
    "0.1".optHexInt should be(None)
    "IX".optHexInt should be(None)
    "full fathom five thy father lies".optHexInt should be(None)
  }

  it should "convert valid hex strings to BigInts" in {
    "0".optHexBigInt.value.intValue should equal(0)
    "100".optHexBigInt.value.intValue should equal(256)
    "-ff".optHexBigInt.value.intValue should equal(-255)
    "12345678".optHexBigInt.value.intValue should equal(0x12345678)
    "-12345678".optHexBigInt.value.intValue should equal(-0x12345678)
    /* More than 4 bytes */
    "FF1020304050".optHexBigInt.value.toString(0x10) should equal("ff1020304050")
    "-FF1020304050".optHexBigInt.value.toString(0x10) should equal("-ff1020304050")
  }

  it should "not convert invalid hex integer strings to BigInts" in {
    (null: String).optHexBigInt should be(None)
    "".optHexBigInt should be(None)
    "NaN".optHexBigInt should be(None)
    "0xG".optHexBigInt should be(None)
    "0.1".optHexBigInt should be(None)
    "IX".optHexBigInt should be(None)
    "full fathom five thy father lies".optHexBigInt should be(None)
  }
}
