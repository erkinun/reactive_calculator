package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class CalculatorSuite extends FunSuite with ShouldMatchers {

  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = TweetLength.tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = TweetLength.tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = TweetLength.tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }


  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = TweetLength.colorForRemainingCharsCount(Var(52))
    assert(resultGreen1() == "green")
    val resultGreen2 = TweetLength.colorForRemainingCharsCount(Var(15))
    assert(resultGreen2() == "green")

    val resultOrange1 = TweetLength.colorForRemainingCharsCount(Var(12))
    assert(resultOrange1() == "orange")
    val resultOrange2 = TweetLength.colorForRemainingCharsCount(Var(0))
    assert(resultOrange2() == "orange")

    val resultRed1 = TweetLength.colorForRemainingCharsCount(Var(-1))
    assert(resultRed1() == "red")
    val resultRed2 = TweetLength.colorForRemainingCharsCount(Var(-5))
    assert(resultRed2() == "red")
  }

  test("computeValues with a double val") {
    val expr: Expr = Literal(5)
    val result = Calculator.computeValues(Map("hede" -> Signal(expr)))

    assert(result("hede")().compareTo(5.0) == 0)
  }

  test("computeValues sum with a double val") {
    val expr: Expr = Literal(5)
    val expr2: Expr = Literal(10)
    val sum: Expr = Plus(expr, expr2)
    val result = Calculator.computeValues(Map("hede" -> Signal(sum)))

    assert(result("hede")().compareTo(15.0) == 0)
  }

  test("computeValues cyclic dependencies") {
    val result = Calculator.computeValues(Map(
      "hede" -> Signal(Ref("hodo")),
      "hodo" -> Signal(Ref("hede"))
    ))

    assert(result("hede")().compareTo(Double.NaN) == 0)
  }

  test("computeValues cyclic dependencies with plus") {
    val result = Calculator.computeValues(Map(
      "hede" -> Signal(Ref("hodo")),
      "hodo" -> Signal(Plus(Literal(1), Ref("hede")))
    ))

    assert(result("hede")().compareTo(Double.NaN) == 0)
  }

  test("computeValues complex cyclic dependencies with plus") {
    val result = Calculator.computeValues(Map(
      "hede" -> Signal(Ref("hodo")),
      "hodo" -> Signal(Plus(Literal(1), Ref("hadi"))),
      "hadi" -> Signal(Minus(Ref("hede"), Literal(5)))
    ))

    assert(result("hede")().compareTo(Double.NaN) == 0)
  }
}
