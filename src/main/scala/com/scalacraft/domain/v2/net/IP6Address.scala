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
package com.scalacraft.domain.v2.net

import com.scalacraft.domain.v2.binary.OctetPair
import com.scalacraft.domain.v2.internal.Information

/**
 * An `IP6Address` represents an IP6 address.
 *
 * The address is comprised of 8 constrained octet pairs.
 *
 * The rules concerning valid string representations for ip6 addresses are taken from http://en.wikipedia.org/wiki/IPv6_address.
 *
 * === Pattern Matching ===
 *
 * Pattern matching is supported as the following example demonstrates,
 * {{{
 * &qt;01::0006:0010&qt; match {
 *   case IP6Address(op1, op2, _, _, _, _, _, op8) =>
 *   (op1, op2, op8) // 0x0001, 0x0000, 0x0010
 *   case _ => None
 * }
 * }}}
 *
 * === Limitations ===
 *
 * The special format that allows for a trailing dotted quad for the rightmost two 16 bit fields is not supported.
 *
 *
 * As shown in the example above the shortening of groups of zeros with a double colon is recognised.
 *
 * TODO: A conversion to the unconstrained version of this class is also available.
 */
case class IP6Address private(
                               field1: OctetPair,
                               field2: OctetPair,
                               field3: OctetPair,
                               field4: OctetPair,
                               field5: OctetPair,
                               field6: OctetPair,
                               field7: OctetPair,
                               field8: OctetPair
                               ) {

  /**
   * Provide a string representation of this ip6 address in RFC5952 recommended form.
   * - colon separator
   * - lowercase hexadecimal digits
   * - no leading zero digits
   * - leftmost longest sequence of two or more zero groups abbreviated with double colon
   * @example 0:10:20:::dd01:3
   * @return A string representation using a colon separator, lowercase hexadecimal digits without leading
   */
  def representation: String = IP6Address.representation(this)

}

object IP6Address {

  private case class Repeated(octetPair: OctetPair, repeatCount: Int)

  private object Sentinel

  private def representation(ip6Address: IP6Address): String = {
    val s = (op: OctetPair) => OctetPair.`to-Int`(op).formatted("%x")
    val fs =
      ip6Address.field1 :: ip6Address.field2 :: ip6Address.field3 :: ip6Address.field4 ::
        ip6Address.field5 :: ip6Address.field6 :: ip6Address.field7 :: ip6Address.field8 :: Nil
    val runs = groupZeroes(fs, Nil)
    val debugText = runs map {
      case Repeated(`zero`, n) => s"($n x 0)"
      case Repeated(group, 1) => s(group)
    }
    println("runs: " + runs)
    println("debugText: " + debugText)

    /**
     * Create the representation without resorting to postprocessing.
     *
     * Split into groups of three to provide context.
     * For each group examine the middle octetpair run.
     * map to one of: "", ":", hexadecimal number.
     * It is then enough to use mkString ":" to complete the representation.
     */
    val contexts = ((Sentinel :: (runs :+ Sentinel)) sliding 3).toList
    contexts foreach (println)
    val reps: List[String] = contexts map {
      case Sentinel :: Repeated(`zero`, _) :: Sentinel :: Nil => "::"
      case Sentinel :: Repeated(`zero`, n) :: _ if n > 1 => ":"
      case _ :: Repeated(`zero`, n) :: Sentinel :: Nil if n > 1 => ":"
      case Repeated(_, 1) :: Repeated(`zero`, n) :: Repeated(_, 1) :: Nil if n > 1 => ""
      case _ :: Repeated(x, 1) :: _ => s(x)
    }

    reps mkString IP6Address.GroupSeparator
  }

  /**
   * Replace runs of zeroes with an single object suitable for later expansion
   * TODO: Documentation
   * @param fields
   * @return
   */
  private def groupZeroes(fields: List[OctetPair], acc: List[Repeated]): List[Repeated] = {
    fields match {
      case Nil => acc.reverse
      case `zero` :: rest => acc match {
        case Repeated(`zero`, n) :: accRest => groupZeroes(rest, Repeated(zero, n + 1) :: accRest)
        case _ => groupZeroes(rest, Repeated(zero, 1) :: acc)
      }
      case f :: fs => groupZeroes(fs, Repeated(f, 1) :: acc)
    }
    //    fields map (f => Repeated(f, 1))
  }

  def opt(
           o1: OctetPair,
           o2: OctetPair,
           o3: OctetPair,
           o4: OctetPair,
           o5: OctetPair,
           o6: OctetPair,
           o7: OctetPair,
           o8: OctetPair
           ): Option[IP6Address] =
    Information.whenNotNull(o1, o2, o3, o4, o5, o6, o7, o8)(apply)

  def opt(x: String): Option[IP6Address] = {
    val allTokens: Option[List[Token]] = x match {
      case Information.Zero() => None
      case _ => Some(parseTokens(x.toLowerCase, Nil))
    }
    println("x: '" + x + "'")
    println(s"allTokens: $allTokens")
    val tokens = for {
      ts <- allTokens
      digitsCount = countDigitGroups(ts)
      zeros = makeZeroes(digitsCount)
      abbreviationCount = countAbbreviations(ts)
      /* Do not allow an abbreviation to be used unnecessarily */
      if abbreviationCount == 0 || digitsCount < RequiredGroupCount
      vs = ts match {
        case AB :: Nil => zeros.init
        case AB :: rest => zeros ++ rest
        case other => other
      }
      us = vs.reverse match {
        case AB :: rest => zeros ++ rest
        case other => other
      }
      /* Replace internal abbreviations */
      ss = us.flatMap {
        case AB => S :: zeros
        case t => t :: Nil
      }
    } yield ss

    println(s"tokens: $tokens")

    for {
      D(d1) :: S :: D(d2) :: S :: D(d3) :: S :: D(d4) :: S :: D(d5) :: S :: D(d6) :: S :: D(d7) :: S :: D(d8) :: Nil <- tokens
      op1 <- OctetPair.opt(d1)
      op2 <- OctetPair.opt(d2)
      op3 <- OctetPair.opt(d3)
      op4 <- OctetPair.opt(d4)
      op5 <- OctetPair.opt(d5)
      op6 <- OctetPair.opt(d6)
      op7 <- OctetPair.opt(d7)
      op8 <- OctetPair.opt(d8)
    }
    yield IP6Address(op1, op2, op3, op4, op5, op6, op7, op8)
  }

  def unapply(candidate: String): Option[(OctetPair, OctetPair, OctetPair, OctetPair, OctetPair, OctetPair, OctetPair, OctetPair)] =
    opt(candidate) map {
      ip6 => (
        ip6.field1,
        ip6.field2,
        ip6.field3,
        ip6.field4,
        ip6.field5,
        ip6.field6,
        ip6.field7,
        ip6.field8
        )
    }

  private val Some(zero) = OctetPair.opt(0)

  private val RequiredGroupCount = 8

  private def countDigitGroups(tokens: Seq[Token]) =
    tokens.count {
      case D(_) => true
      case _ => false
    }

  private def countAbbreviations(tokens: Seq[Token]) = tokens.count(_ == AB)

  private def makeZeroes(count: Int): List[Token] =
    ((1 to (RequiredGroupCount - count)) flatMap { case _ => D("0") :: S :: Nil}).toList

  private def parseTokens(x: String, acc: List[Token]): List[Token] =
    nextToken(x) match {
      case Some((token, rest)) => parseTokens(rest, token :: acc)
      case None => acc
    }

  private val ColonColon = "::(.*)".r
  private val Colon = ":(.*)".r
  private val Digits = "([0-9a-f]+)(.*)".r

  private def nextToken(x: String): Option[(Token, String)] = {
    x match {
      case ColonColon(rest) => Some((AB, rest))
      case Colon(rest) => Some((S, rest))
      case Digits(digits, rest) => Some((D(digits), rest))
      case _ => None
    }
  }

  sealed trait Token

  /* The colon separator between octet pairs */
  case object S extends Token

  /* The abbreviation used to represent a group of zero or more zeroes */
  case object AB extends Token

  case class D(digits: String) extends Token

  private val GroupSeparator = ":"
}