package com.github.mideo.aoc

import com.github.mideo.aoc.Direction.Up

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParSeq


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
                 trail: Seq[(Position, Direction)]):

  private def tryMove(position: Position, direction: Direction): (Position, Direction, Seq[(Position, Direction)]) = {
    puzzle.charAt(position) match {
      case '#' => (trail.last._1, Turn.rightOf(direction), trail.init)
      case _ =>
        val p = Position(position.y + direction.y, position.x + direction.x)
        (p, direction, trail :+ (position, direction))
    }
  }

  @tailrec
  final def move(): Guard = {
    if (!puzzle.withinBoundary(position.y, position.x)) return Guard(position, direction, puzzle, trail)
    val (p, d, t) = tryMove(position, direction)
    Guard(p, d, puzzle, t).move()

  }

object Day6 extends AdventOfCodeExercise[Int]:

  private def findGuard(puzzle: Puzzle): Option[Position] = puzzle.find('^')
    .map(Position.apply)

  override def partOneSolution(input: Seq[String]): Int =
    val puzzle = new Puzzle(input.map(_.toCharArray))
    val trail: Option[Seq[(Position, Direction)]] = findGuard(puzzle)
      .map(position => Guard(position, Direction.Up, puzzle, Seq.empty).move().trail.tail)
    trail match
      case Some(seq) => seq.distinctBy(_._1).size
      case None => 0

  @tailrec
  private final def isLooped(start: Position,
                             obstacle: Position,
                             direction: Direction,
                             visited: Set[(Position, Direction)],
                             puzzle: Puzzle): Boolean = {
    val pair = (start, direction)
    val pos = Position(start.y + direction.y, start.x + direction.x)
    if (visited.contains(pair)) return true
    if (!puzzle.withinBoundary(pos.y, pos.x)) return false

    if (puzzle.charAt(pos) == '#' || pos == obstacle) {
      isLooped(start, obstacle, Turn.rightOf(direction), visited, puzzle)
    } else {
      isLooped(pos, obstacle, direction, visited + pair, puzzle)
    }
  }

  override def partTwoSolution(input: Seq[String]): Int = {

    val puzzle = new Puzzle(input.map(_.toCharArray))
    findGuard(puzzle)
      .map {
        start =>
          val guard = Guard(start, Direction.Up, puzzle, Seq.empty).move()
          val trail: ParSeq[Position] = guard.trail.tail.init.map(_._1).distinct.toList.par
          trail.filter(p => isLooped(start, p, Up, Set(), puzzle)).distinct.size

      }.getOrElse(0)
  }
