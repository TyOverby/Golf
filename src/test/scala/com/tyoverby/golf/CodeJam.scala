package com.tyoverby.golf


import com.tyoverby.golf.implicits._
import com.tyoverby.golf.StringModifiers._

object CodeJam extends App {
  def storeCredit(file: String): String = {
    val lines = splitOn("\n")(getFromFile(file)).tail
    val tokenated = lines.map(splitThenInt(" "))
    val grouped = tokenated.grouped(3).toList.map(_.toTuple3)
    val groupedTrimmed = grouped.map {
      case (a, b, c) => (a.head, c)
    }

    val output = groupedTrimmed.map {
      case (credit, items) => {
        val itemArray = items.toArray
        val Some(List(a, b)) = items.combinations(2).find(_.sum == credit)

        "" + (itemArray.indexOf(a) + 1) + " " + (itemArray.lastIndexOf(b) + 1)
      }
    }.zipWithIndex.map {
      case (v, index) => "Case #" + (index + 1) + ": " + v
    }.mkString("\n")

    output
  }

  def reverseWords(file: String): String = {
    val slurped = getFromFile(file)
    val lines = splitOn("\n", " ")(slurped).tail
    val reved = lines.map(_.reverse)

    val toWrite = reved.zipWithIndex.map {
      case (v, index) => "Case #" + (index + 1) + ": " + v.mkString(" ")
    }.mkString("\n")

    writeToFile(toWrite, "out.txt")

    toWrite
  }

  def texting(file: String): String = {
    val slurped = getFromFile(file)
    val lines = splitOn("\n")(slurped).tail

    val char2digits: Map[Char, Seq[Int]] = Map(
      'a' -> Seq(2),
      'b' -> Seq(2, 2),
      'c' -> Seq(2, 2, 2),
      'd' -> Seq(3),
      'e' -> Seq(3, 3),
      'f' -> Seq(3, 3, 3),
      'g' -> Seq(4),
      'h' -> Seq(4, 4),
      'i' -> Seq(4, 4, 4),
      'j' -> Seq(5),
      'k' -> Seq(5, 5),
      'l' -> Seq(5, 5, 5),
      'm' -> Seq(6),
      'n' -> Seq(6, 6),
      'o' -> Seq(6, 6, 6),
      'p' -> Seq(7),
      'q' -> Seq(7, 7),
      'r' -> Seq(7, 7, 7),
      's' -> Seq(7, 7, 7, 7),
      't' -> Seq(8),
      'u' -> Seq(8, 8),
      'v' -> Seq(8, 8, 8),
      'w' -> Seq(9),
      'x' -> Seq(9, 9),
      'y' -> Seq(9, 9, 9),
      'z' -> Seq(9, 9, 9, 9),
      ' ' -> Seq(0)
    )


    val all = lines.map {
      str => {
        val flattened = str.toList.map(char2digits).foldLeft(List.empty[Int])((builder, in) => if (!builder.isEmpty && builder.last == in.head) builder ++ Seq(-1) ++ in else builder ++ in)
        val toS = flattened.map(x => if (x == -1) " " else x.toString).mkString("")
        toS
      }
    }

    val ccase = new CaseIterator

    val toWrite = all.map {
      value => ccase + value
    }.mkString("\n")
    writeToFile(toWrite, "out.txt")
    toWrite
  }

  println(texting("src/test/resources/texting/C-large-practice.in"))

}
