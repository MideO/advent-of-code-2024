package com.github.mideo.aoc


object Day2 extends AdventOfCodeExercise[Int]:
  override val partOneFile: String = "Day2.txt"
  override val partTwoFile: String = partOneFile

  override def partOneSolution(input: Seq[String]): Int =
    input
      .map(line => Levels(line.split(" ").map(_.toInt)))
      .count(_.isSafe)

  override def partTwoSolution(input: Seq[String]): Int =
    input
      .map(line => Levels(line.split(" ").map(_.toInt)))
      .count(_.isSafeWithDampener)

  private case class Levels(levels: Seq[Int]):
    def isSafe: Boolean =
      val pairs = {
        if (levels.head > levels.last)
          levels.sliding(2)
        else
          levels.reverse.sliding(2)
      }
      pairs.forall {
        x => x.head - x.last > 0 && x.head - x.last < 4
      }

    def isSafeWithDampener: Boolean =
      levels.indices
        .map(i => Levels(levels.take(i) ++ levels.drop(i + 1)))
        .exists(_.isSafe)