package com.github.mideo.aoc


object Day4 extends AdventOfCodeExercise[Int]:

  override def partOneSolution(input: Seq[String]): Int =
    val word = "XMAS"
    val puzzle = new Puzzle(input.map(_.toCharArray))
    Iterator.tabulate(puzzle.height, puzzle.width) {
      (y, x) =>
        Direction
          .values
          .count(direction => puzzle.exploreDirections(y, x, direction).take(word.length).mkString == word)
    }.flatten.sum


  override def partTwoSolution(input: Seq[String]): Int =
    val word = "MAS"
    val diagonals = Seq(
      (Direction.UpLeft, Direction.DownRight),
      (Direction.UpRight, Direction.DownLeft),
      (Direction.DownRight, Direction.UpLeft),
      (Direction.DownLeft, Direction.UpRight)
    )
    val puzzle = new Puzzle(input.map(_.toCharArray))
    Iterator.tabulate(puzzle.height, puzzle.width) {
      (y, x) =>
        puzzle.charAt(y, x) match
          case 'A' =>
            diagonals.count(
              (coordinate, direction) => puzzle
                .exploreDirections(y + coordinate.y, x + coordinate.x, direction)
                .take(word.length)
                .mkString == word) == 2

          case _ => false

    }.flatten.count(_ == true)


