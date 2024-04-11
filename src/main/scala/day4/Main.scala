package day4

import io.Source
import scala.util.Using
import scala.math

import cats.syntax.all._
import cats.data.EitherT


@main def day4 =

  val input = """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"""
  printCalced(calc(input.split("\n").iterator))
  Using(Source.fromFile("./src/main/resources/day4.txt")) { file =>
    printCalced(calc(file.getLines()))
  }


def printCalced(calculation: Either[String, Double]) = calculation match {
  case Right(points) => println(points)
  case Left(e)       => println("Error: " + e)
}


def calc(lines: Iterator[String]): Either[String, Double] =
  
    lines.toList.map { line => 
        val results = for {
          numParts <- {
            line.split('|') match {
              case Array(winPart: String, guessPart: String) => Right(winPart, guessPart)
              case invalidPattern                            => Left(s"Invalid card pair $invalidPattern")
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
          findedWinnableNums <- Right {
            val winnableCount = guess.intersect(winnable).length
            if (winnableCount == 0)
              0
            else
              math.pow(2, winnableCount - 1)
          }
        } yield (findedWinnableNums)
        results
    }
    .traverse(identity)
    .map(_.sum)


def parseNums(nums: String) = 
    nums.trim
      .split(" ")
      .filter(_.nonEmpty)
      .map(v => v.toIntOption match {
        case Some(v) => v.asRight
        case None    => s"Invalid Int value ${v}".asLeft
      })
      .toList
      .traverse(identity)

