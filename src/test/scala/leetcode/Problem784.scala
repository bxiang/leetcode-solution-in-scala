package leetcode

import org.scalatest.FunSuite

class Problem784 extends FunSuite {

  def doCasePermut(input: List[Char]): List[List[Char]] = {
    input match {
      case Nil => List.empty
      case x :: Nil =>
        if (x.isLetter) List(List(x.toUpper), List(x.toLower)) else (List(List(x)))
      case x :: xs =>
        val charList = if (x.isLetter) List(x.toUpper, x.toLower) else List(x)
        for {
          str <- doCasePermut(xs)
          c <- charList
        } yield c :: str
    }
  }

  def letterCasePermutation(input: String): List[String] = doCasePermut(input.toList).map(_.mkString)

  test("letterCasePermutation") {
    val expectedResult = List("AB3C", "aB3C", "Ab3C", "ab3C", "AB3c", "aB3c", "Ab3c", "ab3c")
    assert(letterCasePermutation("ab3c") == expectedResult)
  }

}