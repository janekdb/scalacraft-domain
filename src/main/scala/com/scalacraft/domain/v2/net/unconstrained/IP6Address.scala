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
package com.scalacraft.domain.v2.net.unconstrained

import com.scalacraft.domain.v2.binary.{OctetPair => ConstrainedOctetPair}
import com.scalacraft.domain.v2.binary.unconstrained.OctetPair
import com.scalacraft.domain.v2.internal.{Information, RejectNullConstructorArgument}
import com.scalacraft.domain.v2.net.{IP6Address => Constrained}

/**
 * `IP6Address`
 * TODO: Documentation
 */
case class IP6Address(
                       octetPairs: List[OctetPair]
                       ) {
  RejectNullConstructorArgument(octetPairs, "octetPairs")
  RejectNullConstructorArgument.rejectNullElement(octetPairs, "octetPairs")

  // TODO: Add and use OctetPair.constrained
  /**
   * Convert to the constrained version of ip6 address.
   * @return An constrained instance of ip6 address as a some or none if this instance
   *         does not convert to a constrained instance
   */
  def constrained: Option[Constrained] = {
    val constrainedOctetPairs: List[Option[ConstrainedOctetPair]] = octetPairs map (OctetPair.`to-Option[OctetPair]`(_))
    type C = ConstrainedOctetPair
    for {
      Some(o1: C) :: Some(o2: C) :: Some(o3: C) :: Some(o4: C) :: Some(o5: C) :: Some(o6: C) :: Some(o7: C) :: Some(o8: C) :: Nil <- Some(constrainedOctetPairs)
      ip6Address <- Constrained.opt(o1, o2, o3, o4, o5, o6, o7, o8)
    }
      yield ip6Address
  }

}

object IP6Address {

  def opt(x: String): Option[IP6Address] = {
    val allTokens: Option[List[Token]] = x match {
      case Information.Zero() => None
      case _ => parseTokens(x.toLowerCase, Nil)
    }
    opt(allTokens)
  }

  private def opt(allTokens: Option[List[Token]]): Option[IP6Address] = {
    val elems: Option[List[Any]] =
      for {
        tokens <- allTokens
        ts = tokens map {
          case D(x) => OctetPair.opt(x)
          case other => other
        }
        digitsCount = countOctetPairs(ts)
        abbreviationCount = countAbbreviations(ts)
        /* Do not allow abbreviations to be used when more than one possible expansion exists. */
        if abbreviationCount <= 1
        zeros = makeZeroes(digitsCount)
        /* Replace leading abbreviations */
        vs = ts match {
          case AB :: Nil => zeros.init // drop trailing separator
          case AB :: rest => zeros ++ rest
          case other => other
        }
        /* Replace trailing abbreviations */
        us = vs.reverse match {
          case AB :: rest => zeros ++ rest
          case other => other
        }
        /* Replace internal abbreviations */
        ss = us.flatMap {
          case AB => S :: zeros
          case t => t :: Nil
        }
      //      // Need to match D [S D}*
      //      Alternating(octetPairs) <- ss
      }
        yield ss
    // Need to match D [S D}*
    for {
      Alternating(octetPairs) <- elems
    }
      yield IP6Address(octetPairs)
  }

  // TODO: Refactor with constrained version
  private def countAbbreviations(tokens: Seq[Any]) = tokens.count(_ == AB)

  private val zero = OctetPair.opt(0)

  // TODO: Refactor with constrained version
  private val RequiredGroupCount = 8

  private def countOctetPairs(tokens: Seq[Any]) =
    tokens.count {
      case Some(_: OctetPair) => true
      case _ => false
    }

  // TODO: Refactor with constrained version
  /**
   * @param digitsCount The number of digits group present
   * @return A list of D(0), S, ... sufficiently long to ensure a list of 8 digit groups when
   *         combined with the existing digit groups
   */
  private def makeZeroes(digitsCount: Int): List[Any] =
    ((1 to (RequiredGroupCount - digitsCount)) flatMap { case _ => zero :: S :: Nil }).toList

  private object Alternating {
    /** @return An octet pair list iff `elems` is alternating sequence of digits and separators bookended with digits: D, S, D, S, ..., D */
    def unapply(elems: List[Any]): Option[List[OctetPair]] = collect(elems, Nil)

    private def collect(elems: List[Any], acc: List[OctetPair]): Option[List[OctetPair]] = {
      elems match {
        case Nil => Some(acc.reverse)
        case Some(op: OctetPair) :: Nil => collect(Nil, op :: acc)
        case Some(op: OctetPair) :: S :: Some(op2: OctetPair) :: rest => collect(Some(op2) :: rest, op :: acc)
        case _ => None
      }
    }
  }

  def unapply(candidate: String): Option[List[OctetPair]] = opt(candidate) map (_.octetPairs)

  // TODO: Refactor parsing with constrained version
  private def parseTokens(x: String, acc: List[Token]): Option[List[Token]] =
    nextToken(x) match {
      case (Some(token), rest) => parseTokens(rest, token :: acc)
      case (None, rest) if rest.isEmpty => Some(acc)
      case (None, _) => None
    }

  private val ColonColon = "::(.*)".r
  private val Colon = ":(.*)".r
  private val Digits = "(-?[0-9a-f]++)(.*)".r

  private def nextToken(x: String): (Option[Token], String) = {
    x match {
      case ColonColon(rest) => (Some(AB), rest)
      case Colon(rest) => (Some(S), rest)
      case Digits(digits, rest) => (Some(D(digits)), rest)
      case _ => (None, x)
    }
  }

  sealed trait Token

  /* The colon separator between octet pairs */
  case object S extends Token

  /* The abbreviation used to represent a group of zero or more zeroes */
  case object AB extends Token

  case class D(digits: String) extends Token

//  private val GroupSeparator = ":"
}