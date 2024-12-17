package com.github.mideo.aoc

import scala.util.matching.Regex

sealed trait GraphItem

case class Edge(node: Int, child: Int) extends GraphItem

case class Path(nodes: Int*) extends GraphItem


object Day5 extends AdventOfCodeExercise[Int]:
  private val edgeRegex: Regex = """^(\d+)\|(\d+)$""".r
  private val pathRegex: Regex = """^(\d.+)""".r

  override def partOneSolution(input: Seq[String]): Int =
    val items: Map[String, Seq[GraphItem]] = buildItemsMap(input)
    val nodeAndEdgesMap: Map[Int, Seq[Int]] = buildNodeAndEdgesMap(items(classOf[Edge].getSimpleName))
    items(classOf[Path].getSimpleName)
      .map(_.asInstanceOf[Path])
      .filter(p => p.nodes == p.nodes.sorted(ordering(nodeAndEdgesMap)))
      .map(p => p.nodes(p.nodes.size / 2))
      .sum


  override def partTwoSolution(input: Seq[String]): Int =
    val items: Map[String, Seq[GraphItem]] = buildItemsMap(input)
    val nodeAndEdgesMap: Map[Int, Seq[Int]] = buildNodeAndEdgesMap(items(classOf[Edge].getSimpleName))
    items(classOf[Path].getSimpleName)
      .map(_.asInstanceOf[Path])
      .filterNot(p => p.nodes == p.nodes.sorted(ordering(nodeAndEdgesMap)))
      .map(path => Path(path.nodes.sorted(ordering(nodeAndEdgesMap)): _*))
      .map(p => p.nodes(p.nodes.size / 2))
      .sum


  private def ordering(nodeAndEdgesMap: Map[Int, Seq[Int]]): Ordering[Int] = {
    (x: Int, y: Int) => {
      if (nodeAndEdgesMap.get(x).exists(_.contains(y))) -1 else 1
    }
  }

  private def buildNodeAndEdgesMap(items: Seq[GraphItem]): Map[Int, Seq[Int]] = {
    items
      .groupBy(_.asInstanceOf[Edge].node).asInstanceOf[Map[Int, Seq[Edge]]]
      .map((k, v) => (k, v.map(_.child)))
  }

  private def buildItemsMap(input: Seq[String]) = {
    input.filter(_.nonEmpty).map {
      case edgeRegex(x, y) => Edge(x.toInt, y.toInt)
      case pathRegex(x) => Path(x.split(",").map(_.toInt): _*)
    }.groupBy(_.getClass.getSimpleName)
  }

