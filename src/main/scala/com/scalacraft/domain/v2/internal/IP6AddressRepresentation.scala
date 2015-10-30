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
 * Create a representation from the fields of an IP6 address.
 */
object IP6AddressRepresentation {

  private val Zero = "0"

  private case class Repeated(field: String, repeatCount: Int)

  private object Sentinel

  def representation(fields: List[String]): String = {

    val runs = groupZeroes(fields, Nil)

    /* Convert all zero runs back to full runs except for the leftmost longest run */

    val maxRunLength = runs.map { case Repeated(_, n) => n }.max

    val maximumLengthRun = Repeated(Zero, maxRunLength)

    val leftmostLongestRun = runs indexOf maximumLengthRun

    /**
     * The winner is the group that is the leftmost of the longest groups of zeroes.
     * Isolate the winner to first group.
     * For the cases when there is no winner or the winner is the head of the list the corresponding indcies are
     * 0 or 1 and in both case the winner will not be in the second group.
     */
    val (winnerGroup, loserGroup) = runs splitAt (leftmostLongestRun + 1)

    val unrollAll: PartialFunction[Repeated, List[Repeated]] = {
      case Repeated(op, n) => List.fill(n)(Repeated(op, 1))
    }

    val preserveLongest: PartialFunction[Repeated, List[Repeated]] = {
      case a@`maximumLengthRun` => a :: Nil
    }

    /* Convert all zero groups other than the first longest to a full run */

    val left: List[Repeated] = winnerGroup flatMap (preserveLongest orElse unrollAll)

    val right: List[Repeated] = loserGroup flatMap unrollAll

    val unrolledRuns = left ++ right

    /**
     * Create the representation without resorting to postprocessing.
     *
     * Split into groups of three to provide context.
     * For each group examine the middle octetpair run.
     * map to one of: "", ":", hexadecimal number.
     * It is then enough to use mkString ":" to complete the representation.
     */
    val contexts = ((Sentinel :: (unrolledRuns :+ Sentinel)) sliding 3).toList

    val reps: List[String] = contexts map {
      case Sentinel :: Repeated(Zero, _) :: Sentinel :: Nil => "::"
      case Sentinel :: Repeated(Zero, n) :: _ if n > 1 => ":"
      case _ :: Repeated(Zero, n) :: Sentinel :: Nil if n > 1 => ":"
      case _ :: Repeated(Zero, n) :: _ if n > 1 => ""
      case _ :: Repeated(x, 1) :: _ => x
    }

    reps mkString GroupSeparator
  }

  /**
   * Replace runs of zeroes with an single object counting the number of zeros in the run.
   * @param fields The fields to accumulate into the runs
   * @return A list containing one item per contiguous group of zeroes and one item for each non-zero
   *         octet pair
   */
  private def groupZeroes(fields: List[String], acc: List[Repeated]): List[Repeated] = {
    fields match {
      case Nil => acc.reverse
      case Zero :: rest => acc match {
        case Repeated(Zero, n) :: accRest => groupZeroes(rest, Repeated(Zero, n + 1) :: accRest)
        case _ => groupZeroes(rest, Repeated(Zero, 1) :: acc)
      }
      case f :: fs => groupZeroes(fs, Repeated(f, 1) :: acc)
    }
  }

  def representationWithoutAbbreviation(fields: List[String]): String = fields mkString GroupSeparator

  private val GroupSeparator = ":"
}
