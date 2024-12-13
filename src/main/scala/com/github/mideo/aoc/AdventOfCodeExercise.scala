package com.github.mideo.aoc

import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

trait AdventOfCodeExercise[K]:
  val partOneFile: String
  val partTwoFile: String


  def partOneSolution(input: Seq[String]): K

  def partTwoSolution(input: Seq[String]): K

  private def readFile(str: String): Try[Seq[String]] = Using(
    Source.fromURL(getClass.getClassLoader.getResource(str))
  ) {
    reader => reader.mkString.split("\n")
  }


  def partOne(): Unit = readFile(partOneFile) match
    case Failure(exception) => throw exception
    case Success(value) => println(partOneSolution(value).toString)


  def partTwo(): Unit = readFile(partOneFile) match
    case Failure(exception) => throw exception
    case Success(value) => println(partTwoSolution(value).toString)