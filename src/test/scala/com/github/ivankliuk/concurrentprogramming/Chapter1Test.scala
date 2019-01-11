package com.github.ivankliuk.concurrentprogramming

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class Chapter1Test extends FunSuite {

  import Chapter1._

  private val zeros = List.fill(5)(0)
  private val matcherWithRegex = matcher("[hH]...[oO]")

  test("`compose` returns function composition") {
    val func = compose[Int, Int, Int](_ + 1, _ * 2)

    func(1) should equal(3)
    func(2) should equal(5)
  }

  test("`fuse` returns Some(A, B)") {
    fuse(Some(1), Some("string")) should equal(Some(1, "string"))
  }

  test("`fuse` returns None") {
    fuse(None, Some("string")) shouldBe None
    fuse(Some(12), None) shouldBe None
  }

  test("`check` doesn't throw an exception") {
    check(0 until 10)(40 / _ > 0) should equal(false)
  }

  test("`check` all elements meet predicate") {
    check(zeros)(_ == 0) should equal(true)
  }

  test("`check` at least one element doesn't meet predicate") {
    check(2 :: zeros)(_ == 1) should equal(false)
    check(2 :: zeros)(_ == 1) should equal(false)
  }

  test("`Pair` class has pattern matching behaviour") {
    val getResult = (a: Any, b: Any) => {
      val pair = new Pair(a, b)
      pair match {
        case Pair(1, "one") => true
        case _ => false
      }
    }

    getResult(1, "one") shouldBe true
    getResult("any", "thing") shouldBe false
    getResult(true, 1.23) shouldBe false
  }

  test("`permutations` should return all permutations of the string") {
    permutations("cow") should equal(Seq("cow", "cwo", "ocw", "owc", "wco", "woc"))
  }

  test("`combinations` should return all combinations length `n` of the sequence") {
    combinations(2, Seq(1, 4, 9, 16)).toList should equal(
      List(
        Seq(1, 4), Seq(1, 9), Seq(1, 16), Seq(4, 9), Seq(4, 16), Seq(9, 16)
      )
    )
  }

  test("`matcher` should return `PartialFunction[String, List[String]]`") {
    matcherWithRegex shouldBe a[PartialFunction[String, List[String]]]
  }

  test("`matcher` `PartialFunction[String, List[String]] should return a list of matches`") {
    matcherWithRegex("hE11oHeLLO") should equal(List("hE11o", "HeLLO"))
  }

  test("`matcher` `PartialFunction[String, List[String]] should throw `MatchError`") {
    an[MatchError] should be thrownBy {
      matcherWithRegex("shouldNotMatch")
    }
  }

}
