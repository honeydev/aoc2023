package day2

import cats.*
import cats.syntax.all.*

import scala.io.Source
import scala.util.Using

enum Color(val maxValue: Int) extends Enum[Color]:
  case Red extends Color(maxValue = 12)
  case Green extends Color(maxValue = 13)
  case Blue extends Color(maxValue = 14)

case class Cube(count: Int, color: Color)

@main def day2 =

  val input = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
"""

  println(calc(input.split("\n").iterator))
  val result = Using(Source.fromFile("./src/main/resources/day2.txt")) { file =>
    calc(file.getLines())
  }
  println(s"Full result: $result")

def calc(rows: Iterator[String]) =
  rows.map(parseRow)
    .toList
    .traverse(identity) match
    case Left(err)    => println(err)
    case Right(games) =>
      games.foldMap {
        // Use cats semigroup implementation for Tuple2[Int, Int]
        case (Some(gId), cp) => (gId, cp)
        case (None, cp)      => (0, cp)
      }

def parseRow(row: String): Either[String, (Option[Int], Int)] =
  val gameLastIndex = row.indexOf(":")
  val firstSpaceIndex = row.indexOf(" ") + 1

  for {
    id <- {
      val intString = row.slice(firstSpaceIndex, gameLastIndex)
      intString.toIntOption.toRight(s"Invalid int value ${intString}")
    }
    cubeRows <- {
      row
        .slice(gameLastIndex + 1, row.length)
        .split(";")
        .flatMap(
          _.trim.split(",").map(v => {
            val Array(c, n) = v.trim.split(" ")
            for {
              count <- c.toIntOption.toRight(s"invalid int $c")
              color <- n match {
                case "red"   => Right(Color.Red)
                case "blue"  => Right(Color.Blue)
                case "green" => Right(Color.Green)
                case v       => Left(s"Invalid color value $v")
              }
            } yield Cube(count, color)
          }))
        .toList
        .traverse(identity)
    }
    cubesPower = {
      cubeRows
        .groupBy(_.color)
        .map { case (k, v) =>
          v.max(Ordering.by[Cube, Int](_.count)).count
        }.product
    }
    gameId = if (cubeRows.forall({ v => v.count <= v.color.maxValue })) Some(id) else None
  } yield (gameId, cubesPower)
