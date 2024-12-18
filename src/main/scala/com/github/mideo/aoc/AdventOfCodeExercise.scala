package com.github.mideo.aoc

import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

trait AdventOfCodeExercise[K]:
  private val inputFile: String = s"${getClass.getSimpleName.replace("$", "")}.txt"

  def partOneSolution(input: Seq[String]): K

  def partTwoSolution(input: Seq[String]): K

  private def readFile(str: String): Try[Seq[String]] = Using(
    Source.fromURL(getClass.getClassLoader.getResource(str))
  ) {
    reader => reader.mkString.split("\n")
  }


  def partOne(): Unit = readFile(inputFile) match
    case Failure(exception) => throw exception
    case Success(value) => println(partOneSolution(value).toString)


  def partTwo(): Unit = readFile(inputFile) match
    case Failure(exception) => throw exception
    case Success(value) => println(partTwoSolution(value).toString)

class Puzzle(val grid: Seq[Seq[Char]]):
  def withinBoundary(y: Int, x: Int): Boolean =
    (y >= 0 && y < grid.length) && (x >= 0 && x < grid(y).length)

  def height: Int = grid.size

  def width: Int = grid.head.size

  def charAt(y: Int, x: Int): Char = grid(y)(x)

  def find(char: Char): Option[(Int, Int)] =
    grid.find(_.contains(char))
      .map(line => (grid.indexOf(line), line.indexOf(char)))


  def exploreDirections(y: Int, x: Int, direction: Direction): Iterator[Char] =
    Iterator.unfold(y, x): (y, x) =>
      Option.when(withinBoundary(y, x))(charAt(y, x), (y + direction.y, x + direction.x))
