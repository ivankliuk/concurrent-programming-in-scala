package com.github.ivankliuk.concurrentprogramming

import scala.util.matching.Regex
import scala.util.Try

/**
  * Chapter 1: Introduction
  */
object Chapter1 {

  /**
    * 1. Implement a [[compose]] method with the following signature:
    *
    * {{{
    * def compose[A, B, C](g: B => C, f: A => B): A => C = ???
    * }}}
    *
    * This method must return a function h, which is the composition of the
    * functions `f` and `g`.
    */
  def compose[A, B, C](g: B => C, f: A => B): A => C = g compose f


  /**
    * 2. Implement a [[fuse]] method with the following signature:
    *
    * {{{
    * def fuse[A, B](a: Option[A], b: Option[B]): Option[(A, B)] = ???
    * }}}
    *
    * The resulting `Option` object should contain a tuple of values from the
    * `Option` objects `a` and `b`, given that both `a` and `b` are non-empty.
    * Use `for` comprehensions.
    */
  def fuse[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    for {
      aa <- a
      bb <- b
    } yield (aa, bb)

  /**
    * 3. Implement a [[check]] method, which takes a set of values of type `T`
    * and a function of type `T => Boolean`:
    *
    * {{{
    * def check[T](xs: Seq[T])(pred: T => Boolean): Boolean = ???
    * }}}
    *
    * The method must return `true` if and only if the `pred` function returns
    * `true` for all the values in `xs` without throwing an exception. Use the
    * [[check]] method as follows:
    *
    * {{{
    * check(0 until 10)(40 / _ > 0)
    * }}}
    */
  def check[T](xs: Seq[T])(pred: T => Boolean): Boolean =
    xs.forall(x => Try(pred(x)).getOrElse(false))

  /**
    * 4. Modify the Pair class from this chapter so that it can be used in
    * a pattern match.
    */
  class Pair[P, Q](val first: P, val second: Q)

  object Pair {
    def unapply[P, Q](pair: Pair[P, Q]): Option[(P, Q)] =
      Some(pair.first, pair.second)
  }

  /**
    * 5. Implement a permutations function, which, given a string, returns
    * a sequence of strings that are lexicographic permutations of the input
    * string:
    *
    * {{{
    * def permutations(x: String): Seq[String] = ???
    * }}}
    */
  def permutations(x: String): Seq[String] = x.permutations.toSeq

  /**
    * 6. Implement a combinations function that, given a sequence of elements,
    * produces an iterator over all possible combinations of length `n`.
    * A combination is a way of selecting elements from the collection so that
    * every element is selected once, and the order of elements does not matter.
    * For example, given a collection `Seq(1, 4, 9, 16)`, combinations of
    * length 2 are `Seq(1, 4)`, `Seq(1, 9)`,
    * `Seq(1, 16)`, `Seq(4, 9)`, `Seq(4, 16)`, and `Seq(9, 16)`.
    * The combinations function has the following signature:
    * {{{
    * def combinations(n: Int, xs: Seq[Int]): Iterator[Seq[Int]]
    * }}}
    */
    def combinations(n: Int, xs: Seq[Int]): Iterator[Seq[Int]] = xs.combinations(n)

  /**
    * 7. Implement a method that takes a regular expression, and returns
    * a partial function from a string to lists of matches within that string:
    * {{{
    * def matcher (regex: String): PartialFunction[String, List[String]]
    * }}}
    * The partial function should not be defined if there are no matches within
    * the argument strings. Otherwise, it should use the regular expression to
    * output the list of matches.
    */
  def matcher(regex: String): PartialFunction[String, List[String]] = {
    val matches: String => Regex.MatchIterator = new Regex(regex).findAllIn _

    // I could've used `PartialFunction.apply()` but it had been deprecated.
    { case x if matches(x).nonEmpty => matches(x).toList }
  }
}
