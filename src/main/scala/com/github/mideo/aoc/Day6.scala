package com.github.mideo.aoc

case class Position(y: Int, x: Int)

case class Guard(position: Position, direction: Direction, puzzle: Puzzle):
  private val rightOf: Map[Direction, Direction] = Map(
    Direction.Up -> Direction.Right,
    Direction.Right -> Direction.Down,
    Direction.Down -> Direction.Left,
    Direction.Left -> Direction.Up
  )

  def move(): Guard = {
    val y = position.y + direction.y
    val x = position.x + direction.x
    if (!puzzle.withinBoundary(x, y))
      return Guard(position, direction, puzzle)
    puzzle.charAt(y, x) match {
      case '#' => Guard(position, rightOf(direction), puzzle).move()
      case _ => Guard(Position(y, x), direction, puzzle)
    }
  }

object Day6 extends AdventOfCodeExercise[Int]:
  private def findGuard(puzzle: Puzzle): Option[Position] = puzzle.find('^')
    .map(Position.apply)

  override def partOneSolution(input: Seq[String]): Int =
    val puzzle = new Puzzle(input.map(_.toCharArray))
    val iterator = findGuard(puzzle) match {
      case Some(position) =>
        Iterator.unfold(Guard(position, Direction.Up, puzzle)):
          g =>
            Option.when(puzzle.withinBoundary(position.y, position.x))(
              g, g.move()
            )
      case None => Iterator.empty
    }
    iterator.iterator
      .take(puzzle.height * puzzle.width)
      .map(_.position)
      .map(position => (position.y, position.x))
      .distinct.toList.length


  override def partTwoSolution(input: Seq[String]): Int =
    0
