package com.tyoverby.golf

import language.implicitConversions

object implicits {
  implicit def list2golfList[A](orig: List[A]): GolfList[A] = GolfList(orig)

  implicit def golfList2List[A](gl: GolfList[A]): List[A] = gl.underlying

}

case class GolfList[+A](underlying: List[A]) {

  import implicits._

  def toGL: GolfList[A] = this

  def groupedBy(groupCount: Int): List[List[A]] = underlying.grouped(groupCount).toList

  def toSingle: A = {
    require(underlying.tail.isEmpty)
    underlying.head
  }

  def toTuple2: (A, A) = underlying match {
    case List(a, b) => (a, b)
    case _ => throw new IllegalArgumentException(f"toTuple2 List was not of size 2: $underlying ")
  }

  def toTuple3: (A, A, A) = underlying match {
    case List(a, b, c) => (a, b, c)
    case _ => throw new IllegalArgumentException(f"toTuple3 List was not of size 3: $underlying ")
  }

  def toTuple4: (A, A, A, A) = underlying match {
    case List(a, b, c, d) => (a, b, c, d)
    case _ => throw new IllegalArgumentException(f"toTuple4 List was not of size 4: $underlying ")
  }

  def toTuple5: (A, A, A, A, A) = underlying match {
    case List(a, b, c, d, e) => (a, b, c, d, e)
    case _ => throw new IllegalArgumentException(f"toTuple5 List was not of size 5: $underlying ")
  }

  def toTuple6: (A, A, A, A, A, A) = underlying match {
    case List(a, b, c, d, e, f) => (a, b, c, d, e, f)
    case _ => throw new IllegalArgumentException(f"toTuple6 List was not of size 6: $underlying ")
  }

  def toTuple7: (A, A, A, A, A, A, A) = underlying match {
    case List(a, b, c, d, e, f, g) => (a, b, c, d, e, f, g)
    case _ => throw new IllegalArgumentException(f"toTuple7 List was not of size 7: $underlying ")
  }

  def toTuple8: (A, A, A, A, A, A, A, A) = underlying match {
    case List(a, b, c, d, e, f, g, h) => (a, b, c, d, e, f, g, h)
    case _ => throw new IllegalArgumentException(f"toTuple8 List was not of size 8: $underlying ")
  }

  def toTuple9: (A, A, A, A, A, A, A, A, A) = underlying match {
    case List(a, b, c, d, e, f, g, h, i) => (a, b, c, d, e, f, g, h, i)
    case _ => throw new IllegalArgumentException(f"toTuple9 List was not of size 9: $underlying ")
  }

  def toTuple10: (A, A, A, A, A, A, A, A, A, A) = underlying match {
    case List(a, b, c, d, e, f, g, h, i, j) => (a, b, c, d, e, f, g, h, i, j)
    case _ => throw new IllegalArgumentException(f"toTuple10 List was not of size 10: $underlying ")
  }

  def toTuple11: (A, A, A, A, A, A, A, A, A, A, A) = underlying match {
    case List(a, b, c, d, e, f, g, h, i, j, k) => (a, b, c, d, e, f, g, h, i, j, k)
    case _ => throw new IllegalArgumentException(f"toTuple11 List was not of size 11: $underlying ")
  }

  def toTuple12: (A, A, A, A, A, A, A, A, A, A, A, A) = underlying match {
    case List(a, b, c, d, e, f, g, h, i, j, k, l) => (a, b, c, d, e, f, g, h, i, j, k, l)
    case _ => throw new IllegalArgumentException(f"toTuple12 List was not of size 12: $underlying ")
  }

  def toTuple13: (A, A, A, A, A, A, A, A, A, A, A, A, A) = underlying match {
    case List(a, b, c, d, e, f, g, h, i, j, k, l, m) => (a, b, c, d, e, f, g, h, i, j, k, l, m)
    case _ => throw new IllegalArgumentException(f"toTuple13 List was not of size 13: $underlying ")
  }

  def toTuple14: (A, A, A, A, A, A, A, A, A, A, A, A, A, A) = underlying match {
    case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
    case _ => throw new IllegalArgumentException(f"toTuple14 List was not of size 14: $underlying ")
  }

  def toTuple15: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A) = underlying match {
    case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
    case _ => throw new IllegalArgumentException(f"toTuple15 List was not of size 15: $underlying ")
  }

  def toTuple16: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A) = underlying match {
    case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
    case _ => throw new IllegalArgumentException(f"toTuple16 List was not of size 16: $underlying ")
  }

  def toTuple17: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A) = underlying match {
    case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
    case _ => throw new IllegalArgumentException(f"toTuple17 List was not of size 17: $underlying ")
  }

  def toTuple18: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A) = underlying match {
    case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
    case _ => throw new IllegalArgumentException(f"toTuple18 List was not of size 18: $underlying ")
  }

  def toTuple19: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A) = underlying match {
    case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
    case _ => throw new IllegalArgumentException(f"toTuple19 List was not of size 19: $underlying ")
  }

  def toTuple20: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A) = underlying match {
    case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
    case _ => throw new IllegalArgumentException(f"toTuple20 List was not of size 20: $underlying ")
  }

  def toTuple21: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A) = underlying match {
    case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
    case _ => throw new IllegalArgumentException(f"toTuple21 List was not of size 21: $underlying ")
  }

  def toTuple22: (A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A, A) = underlying match {
    case List(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
    case _ => throw new IllegalArgumentException(f"toTuple22 List was not of size 22: $underlying ")
  }

}

object Entry extends App {
  val alphabet: List[Char] = "abcdefghijklmnopqrstuvwxyz".toList
  for (i <- 1 to 26) {
    println(s"def toTuple$i:" + Stream.continually(List("A").toStream).flatten.take(i).mkString("(", ",", ")") + " = underlying match{")
    println("case List(" + alphabet.take(i).mkString(",") + ") => " + alphabet.take(i).mkString("(", ",", ")"))
    println("case _ => throw new IllegalArgumentException(f\"toTuple" + i + " List was not of size " + i + ": $underlying \")")

    println("}")
  }
}