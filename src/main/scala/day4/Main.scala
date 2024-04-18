package day4

import cats.implicits.*

import scala.annotation.tailrec
import scala.io.Source
import scala.math
import scala.util.Using


@main def day4 =

  val input = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"""

  printCalced(calc1(input.split("\n").iterator))
  printCalced(calc2(input.split("\n").iterator))
  Using(Source.fromFile("./src/main/resources/day4.txt")) { file =>
    printCalced(calc1(file.getLines()))
    printCalced(calc2(file.getLines()))
  }


def printCalced(calculation: Either[String, Double | Int]) = calculation match {
  case Right(points) => println(points)
  case Left(e)       => println("Error: " + e)
}


def calc1(lines: Iterator[String]): Either[String, Double] =
  lines.toList.map { line =>
      splitNums(line).flatMap { case (winnable, guess) =>
        Right {
          val winnableCount = guess.intersect(winnable).length

          if (winnableCount == 0)
            0
          else
            math.pow(2, winnableCount - 1)
        }
      }
    }
    .traverse(identity)
    .map(_.sum)

private def calc2(lines: Iterator[String]) =
  type CardCount = Int
  type CardPoints = Int
  type Card = (CardCount, CardPoints)

  lines.toList.mapWithIndex { (line, index) =>
      splitNums(line).flatMap { case (winnable, guess) =>
        Right {
          val cardNumber = index + 1
          val winnableCount = guess.intersect(winnable).length
          if (winnableCount == 0)
            (1, 0)
          else
            (1, winnableCount)
        }
      }
    }
    .traverse(identity)
    .map({ v =>
      @tailrec
      def loop(cards: List[Card], acc: Int): Int =
        cards match {
          case h :: tail =>
            val (count, points) = h
            val incremented = tail.slice(0, points).map {
              case (otherCount, otherPoints) => (otherCount + count, otherPoints)
            }
            val other = tail.slice(points, tail.length)
            loop(incremented ++ other, acc + count)

          case _ => acc
        }

      loop(v, 0)
    })

private def splitNums(line: String): Either[String, (List[Int], List[Int])] =
  for {
    numParts <- {
      line.split('|') match {
        case Array(winPart: String, guessPart: String) => Right(winPart, guessPart)
        case invalidPattern => Left(s"Invalid card pair $invalidPattern")
      }
    }
    (winnableNumsPart, guessNumsPart) = numParts
    cardLogoColonIndex = winnableNumsPart.indexOf(":")
    winnable <- parseNums(
      winnableNumsPart.slice(
        cardLogoColonIndex + 1,
        winnableNumsPart.length)
    )
    guess <- parseNums(guessNumsPart)
  } yield (winnable, guess)

private def parseNums(nums: String) =
    nums.trim
      .split(" ")
      .filter(_.nonEmpty)
      .map(v => v.toIntOption match {
        case Some(v) => v.asRight
        case None    => s"Invalid Int value ${v}".asLeft
      })
      .toList
      .traverse(identity)
