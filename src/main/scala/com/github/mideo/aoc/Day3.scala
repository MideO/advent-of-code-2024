package com.github.mideo.aoc

import scala.util.matching.Regex

case class Multiplier(x: Int, y: Int):
  def product: Int = x * y


object Day3 extends AdventOfCodeExercise[Int]:
  private val multiplierPattern: Regex = """mul\((\d+),(\d+)\)""".r
  private val conditionalMultiplierPattern: Regex = """(mul\((\d+),(\d+)\)|do\(\)|don't\(\))""".r


  override def partOneSolution(input: Seq[String]): Int =
    multiplierPattern.findAllIn(input.mkString)
      .map { case multiplierPattern(a, b) => Multiplier(a.toInt, b.toInt).product }
      .sum


  override def partTwoSolution(input: Seq[String]): Int =
    conditionalMultiplierPattern.findAllIn(input.mkString)
      .foldLeft((true, 0)) {
        case ((true, acc), multiplierPattern(a, b)) => (true, acc + Multiplier(a.toInt, b.toInt).product)
        case (x, "don't()") => (false, x._2)
        case (x, "do()") => (true, x._2)
        case (x, y) => x
      }._2



