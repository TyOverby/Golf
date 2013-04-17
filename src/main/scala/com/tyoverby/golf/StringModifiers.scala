package com.tyoverby.golf

import scala.io.Source
import java.io.{BufferedWriter, FileWriter}

object StringModifiers {
  def getFromUrl(url: String): String = Source.fromURL(url).getLines().mkString("\n")

  def getFromFile(path: String): String = Source.fromFile(path).getLines().mkString("\n")

  def splitOn(regex: String)(operand: String): List[String] = operand.split(regex).toList

  def splitOn(r1: String, r2: String)(operand: String): List[List[String]] = splitOn(r1)(operand).map(splitOn(r2))

  def splitOn(r1: String, r2: String, r3: String)(operand: String): List[List[List[String]]] = splitOn(r1)(operand).map(x => splitOn(r2)(x).map(splitOn(r3)))

  def splitThenInt(regex: String)(operand: String): List[Int] = operand.split(regex).map(_.toInt).toList

  def splitThenDouble(regex: String)(operand: String): List[Double] = operand.split(regex).map(_.toDouble).toList

  def allToInt(strings: List[String]): List[Int] = strings.map(_.toInt)

  def allToDouble(strings: List[String]): List[Double] = strings.map(_.toDouble)

  def writeToFile(source: String, fileName: String) {
    val fstream = new FileWriter(fileName)
    val out = new BufferedWriter(fstream)
    out.write(source)
    //Close the output stream
    out.close()
  }
}
