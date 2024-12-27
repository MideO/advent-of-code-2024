package com.github.mideo.aoc


object Day8 extends AdventOfCodeExercise[Int]:
  type ResonantHarmonics = (Position, Position) => List[Position]

  private def findAntennas(puzzle: Puzzle): Map[Char, List[Position]] = puzzle
    .positions
    .toList
    .groupBy(puzzle.charAt)
    .filter((k, v) => Character.isLetterOrDigit(k))

  private def antinodes(position: Position,
                        antennas: List[Position],
                        puzzle: Puzzle,
                        resonantHarmonics: ResonantHarmonics): List[Position] =
    antennas.filterNot(_ == position)
      .flatMap(other => resonantHarmonics(position, other) ++ resonantHarmonics(other, position))


  override def partOneSolution(input: Seq[String]): Int =
    val resonantHarmonics: ResonantHarmonics = (position, other) => List(
      Position(position.y + (position.y - other.y), position.x + (position.x - other.x))
    )
    val puzzle = new Puzzle(input.map(_.toCharArray))
    val antennas = findAntennas(puzzle).map {
      (_, group) => group.flatMap(position => antinodes(position, group, puzzle, resonantHarmonics))
    }
    antennas.flatten.toSet.count(p => puzzle.withinBoundary(p.y, p.x))
  end partOneSolution


  override def partTwoSolution(input: Seq[String]): Int =
    val puzzle = new Puzzle(input.map(_.toCharArray))
    val resonantHarmonics: ResonantHarmonics = (position, other) => Iterator
      .iterate(position)(p => Position(p.y + (position.y - other.y), p.x + (position.x - other.x)))
      .takeWhile(p => puzzle.withinBoundary(p.y, p.x))
      .toList

    val antennas = findAntennas(puzzle).map {
      (_, group) => group.flatMap(position => antinodes(position, group, puzzle, resonantHarmonics))
    }
    antennas.flatten.toSet.count(p => puzzle.withinBoundary(p.y, p.x))
  end partTwoSolution
