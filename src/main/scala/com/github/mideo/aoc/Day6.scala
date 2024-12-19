package com.github.mideo.aoc

import scala.annotation.tailrec

case class Position(y: Int, x: Int)

object Turn:
  val rightOf: Map[Direction, Direction] = Map(
    Direction.Up -> Direction.Right,
    Direction.Right -> Direction.Down,
    Direction.Down -> Direction.Left,
    Direction.Left -> Direction.Up
  )


case class Guard(position: Position,
                 direction: Direction,
                 puzzle: Puzzle,
                 trail: Seq[Position]):
  @tailrec
  final def move(): Guard = {
    (position.y + direction.y, position.x + direction.x) match {
      case (y, x) if !puzzle.withinBoundary(y, x) =>
        Guard(position, direction, puzzle, trail :+ position)
      case (y, x) => puzzle.charAt(y, x) match {
        case '#' => Guard(position, Turn.rightOf(direction), puzzle, trail).move()
        case _ => Guard(Position(y, x), direction, puzzle, trail :+ position)
      }
    }
  }

object Day6 extends AdventOfCodeExercise[Int]:
  private def findGuard(puzzle: Puzzle): Option[Position] = puzzle.find('^')
    .map(Position.apply)

  override def partOneSolution(input: Seq[String]): Int =
    val puzzle = new Puzzle(input.map(_.toCharArray))
    val iterator = findGuard(puzzle) match {
      case Some(position) =>
        Iterator.unfold(Guard(position, Direction.Up, puzzle, Seq.empty)):
          g => Option
            .when(puzzle.withinBoundary(g.position.y, g.position.x))(g, g.move())
      case None => Iterator.empty
    }
    val xx = iterator.iterator
      .take(puzzle.height * puzzle.width)
      .map(g => (g.position.y, g.position.x))
      .distinct.toList

    val pp = puzzle.grid.zipWithIndex.map(
      (y, iy) => {
        y.zipWithIndex.map {
          (x, ix) => if (xx.contains(iy, ix)) 'X' else x
        }
      }
    )
    println(pp.map(_.mkString).mkString("\n"))
    xx.length


  override def partTwoSolution(input: Seq[String]): Int =
    0
