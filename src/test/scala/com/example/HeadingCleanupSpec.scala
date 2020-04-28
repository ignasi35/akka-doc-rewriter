package com.example

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class HeadingCleanupSpec extends FlatSpec with Matchers {
  behavior of "Header Cleanup"

  it should "remove @@@div wrapping around ## signature" in {
    val useCase =
      """@ref1234
      |
      |@@@div { .group-scala }
      |
      |## Signature
      |
      |
      |@@@
      |
      |##Desc1234""".stripMargin

    val prefix = "@ref1234"
    val suffix = "##Desc1234"

    run(useCase, prefix, suffix)

  }

  def run(useCase: String, prefix: String, suffix: String) = {
    Tools.headingCleanup(useCase) should be(s"""$prefix
       |
       |## Signature
       |
       |$suffix""".stripMargin)
  }

}
