package day1

import io.Source
import scala.util.Using

@main def day1 =

  val input = """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
12342456546
abcef
"""

  println(calc(input.split("\n").iterator))
  val result = Using(Source.fromFile("./src/main/resources/day1.txt")) { file =>
    calc(file.getLines())
  }
  println(s"Full result: $result")


def calc(lines: Iterator[String]) =
  lines.map(_.filter(_.isDigit))
    .map { digits =>
      digits.toList match
        case x :: Nil => (x.toString + x.toString).toInt
        case head +: _ :+ last => {
          head.toString + last.toString
        }.toInt
        case _ => 0
    }.sum
