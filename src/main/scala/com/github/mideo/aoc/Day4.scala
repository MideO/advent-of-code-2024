package com.github.mideo.aoc


enum Direction(val y: Int, val x: Int):
  case Up extends Direction(-1, 0)
  case Right extends Direction(0, 1)
  case Down extends Direction(1, 0)
  case Left extends Direction(0, -1)
  case UpRight extends Direction(-1, 1)
  case DownRight extends Direction(1, 1)
  case DownLeft extends Direction(1, -1)
  case UpLeft extends Direction(-1, -1)

class Puzzle(grid: Seq[Seq[Char]]):
  private def withinBoundary(y: Int, x: Int): Boolean =
    (y >= 0 && y < grid.length) && (x >= 0 && x < grid(y).length)

  def height: Int = grid.size

  def width: Int = grid.head.size

  def charAt(y: Int, x: Int) = grid(y)(x)

  def exploreDirections(y: Int, x: Int, direction: Direction): Iterator[Char] =
    Iterator.unfold(y, x): (y, x) =>
      Option.when(withinBoundary(y, x))(charAt(y, x), (y + direction.y, x + direction.x))

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


