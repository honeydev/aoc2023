package day3

import cats.*
import cats.syntax.all.*

import scala.io.Source
import scala.util.Using
// Define number as object that has value as number, X range number of indexes where number
// can compare with symbols, and Y range number of rows where num can compare with synbols
//
// Algorithm
// Parse numbers and symbols in rows, save his indexes, save start range as min index - 1 and end
// range as max index + 1, on X and Y coordinates
// compare symbols and numbers rangers, filter numbers where range compare not found
// calc sum of finded numbers
//
trait Value
case class Symbol(index: Int) extends Value
case class Number(value: Int, xRange: Range) extends Value

trait CurrentNumber
case object NoNumber extends CurrentNumber
case class NumberValue(value: String, startIndex: Int) extends CurrentNumber


case class Parser(
  input: List[Char],
  output: List[Value] = List(),
  currentNumber: CurrentNumber = NoNumber,
  index: Int = 0,
) {

  def parse: List[Value] = {
    currentNumber match {
      case NoNumber => input match {
        case Nil => output
        case h :: tail => h match {
          case '.' => Parser(tail, output, currentNumber, index + 1).parse
          case h if h.isDigit => Parser(
            tail,
            output,
            NumberValue(h.toString(), index), index + 1).parse
          case _              => Parser(
            tail,
            output ++ List(Symbol(index)),
            index = index + 1
          ).parse
        }
      }
      case NumberValue(value, startIndex) => input match {
        case Nil => output
        case h :: tail => h match {
          case '.' => Parser(
            tail,
            output ++ List(Number(value.toInt, startIndex - 1 to index)),
            index = index + 1 
          ).parse
          case v if v.isDigit => Parser(
            tail,
            output,
            currentNumber = NumberValue(value + v, startIndex),
            index + 1
            ).parse
          case _  => Parser(
            tail,
            output
            ++ List(Number(value.toInt, (startIndex - 1) to index), Symbol(index)
            ),
            index = index + 1 
          ).parse
      }
    }
  }
}
}

@main def day3 =

  val input = """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."""


  println(calc(input.split("\n").iterator))

def calc(rows: Iterator[String]) =

  val (numbers, symbols) = rows
    .map(r => Parser(r.toList).parse)
     .toList
     .mapWithIndex { 
       case (values, i) => values.map { v => (i, v) }
     }
     .foldLeft[(List[(Int, Number)], List[(Int, Symbol)])]((List(), List())) { (acc, vals) => {
       val (numbers, symbols) = acc
       val (newNumbers, newSymbols) = vals.foldLeft[(List[(Int, Number)], List[(Int, Symbol)])]((List(), List())) {
         (acc, v) => {
           val (numbers, symbols) = acc
           val (index, value) = v
           value match {
             case n: Number => (numbers ++ List((index, n)), symbols)
             case s: Symbol => (numbers, symbols ++ List((index, s)))
           }
         }
       }
       (numbers ++ newNumbers, symbols ++ newSymbols)
     }
   }
  val filtered = numbers.filter { case (ni, n) => symbols.exists { case (si, s) =>
    val indexDiff = (ni - si).abs

    indexDiff < 2 && n.xRange.contains(s.index)
    }
  }

  filtered.map(_._2.value).sum

def parseRow(row: String): Either[String, (Option[Int], Int)] = ???
