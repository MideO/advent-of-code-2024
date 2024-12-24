package com.github.mideo.aoc

object Day7 extends AdventOfCodeExercise[Long]:
  private type Accumulator = (Long, Long, List[Long]) => List[Long]
  
  private val addMultiplyOps: List[Accumulator] = List(
    (x, y, xs) => (x + y) :: xs,
    (x, y, xs) => (x * y) :: xs
  )
  private val addMultiplyConcatOps = addMultiplyOps :+ ((x, y, xs) => s"$x$y".toLong :: xs)

  private def canBeSolved(result: Long, args: List[Long], accumulators: List[Accumulator]): Boolean = {
    args match
      case x :: Nil => result == x
      case x :: y :: xs => accumulators.exists(operation => canBeSolved(result, operation.apply(x, y, xs), accumulators))
      case Nil => false
  }

  override def partOneSolution(input: Seq[String]): Long = input.map(_.split(": "))
      .filter(split => canBeSolved(split.head.toLong, split.last.split(" ").map(_.toLong).toList, addMultiplyOps))
      .map(_.head.toLong)
      .sum


  override def partTwoSolution(input: Seq[String]): Long = input.map(_.split(": "))
    .filter(split => canBeSolved(split.head.toLong, split.last.split(" ").map(_.toLong).toList, addMultiplyConcatOps))
    .map(_.head.toLong)
    .sum