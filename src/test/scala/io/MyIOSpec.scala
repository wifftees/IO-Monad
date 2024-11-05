package io

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MyIOSpec extends AnyFlatSpec with Matchers {

  private val exception = new RuntimeException("Oops")

  "MyIO" should "handle error from raiseError" in {
    MyIO(1)
      .flatMap[Int](_ => MyIO.raiseError(exception).as(2))
      .as(3)
      .attempt
      .unsafeRunSync shouldBe Left(exception)
  }

  it should "handle error from throw" in {
    MyIO(1)
      .flatMap[Int](_ => MyIO.delay(throw exception).as(2))
      .as(3)
      .attempt
      .unsafeRunSync shouldBe Left(exception)
  }

}
