package com.github.mideo.aoc


object Day1 extends AdventOfCodeExercise[Int]:
  override val inputFile: String = "Day1.txt"

  override def partOneSolution(input: Seq[String]): Int =
    val (head, last) = input
      .map(_.split(" "))
      .foldLeft((List[Int](), List[Int]())) {
        (init, arr) => ((init._1 :+ arr.head.toInt).sorted, (init._2 :+ arr.last.toInt).sorted)
      }
    head.lazyZip(last).foldLeft(0)((init, pair) => init + Math.abs(pair._1 - pair._2))


  override def partTwoSolution(input: Seq[String]): Int =
    val (head, last) = input
      .map(_.split(" "))
      .foldLeft((List[Int](), List[Int]())) {
        (init, arr) => (init._1 :+ arr.head.toInt, init._2 :+ arr.last.toInt)
      }
    val occurrence = last.groupBy(identity).view.mapValues(_.size)
    head.map(x => x * occurrence.getOrElse(x, 0)).sum


  


