package com.example

import java.io.File
import java.nio.file.Path
import java.nio.file.Paths

import com.example.Tools.FileAnchors
import com.example.Tools.FileContents
import com.example.Tools.RewriteCommand
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class ApidocDirectiveBuilder extends FlatSpec with Matchers {

  behavior of "Apidoc directive builder (anchor collapse logic)"

  it should "Return the input when there's no collision (all inputs are classes)" in {
    val input = Seq(("FakeOps", "java=#fakeOp"), ("FakeOps", "scala=#fakeOp"))
    Tools.collapseAnchorsWithObjectPrecedence(input) should be(input)
  }
  it should "Return the input when there's no collision (all inputs are objects)" in {
    val input = Seq(("FakeOps$", "java=#fakeOp"), ("FakeOps$", "scala=#fakeOp"))
    Tools.collapseAnchorsWithObjectPrecedence(input) should be(input)
  }

  it should "Filter class when there's collision" in {
    val input = Seq(
      ("FakeOps", "java=#fakeOp"),
      ("FakeOps$", "java=#fakeOp"),
      ("FakeOps$", "scala=#fakeOp")
    )
    val expected =
      Set(("FakeOps$", "java=#fakeOp"), ("FakeOps$", "scala=#fakeOp"))
    // Convert to Set because order is not important
    Tools.collapseAnchorsWithObjectPrecedence(input).toSet should be(expected)
  }

  behavior of "Apidoc directive builder"

  it should "generate a single @apidoc statement with java and scala" in {
    val key = new File(".", "fakeOp.md").toPath
    val scaladoc = new File(".", "scaladsl/FakerBuilderOps.html").toPath
    val javadoc = new File(".", "javadsl/FakerBuilderOps.html").toPath
    val scalaOps: Map[Path, Seq[Tools.RewriteCommand]] = Map(
      key -> Seq(
        RewriteCommand(
          FileContents(key, Seq.empty),
          "fakeOp",
          FileAnchors(scaladoc, Seq("fakeOp(Int, Option):Unit"))
        )
      )
    )
    val javaOps: Map[Path, Seq[Tools.RewriteCommand]] = Map(
      key -> Seq(
        RewriteCommand(
          FileContents(key, Seq.empty),
          "fakeOp",
          FileAnchors(javadoc, Seq("fakeOp(int, Optional)"))
        )
      )
    )
    Tools.buildApidocStatements(key, scalaOps, javaOps) should be(
      """@apidoc[FakerBuilderOps.fakeOp](FakerBuilderOps) { scala="#fakeOp(Int, Option):Unit" java="#fakeOp(int, Optional)" }"""
    )
  }

  it should "not use the `$` on the title of the link when the target class is an object" in {
    val key = new File(".", "fakeOp.md").toPath
    val scaladoc = new File(".", "scaladsl/FakerBuilderOps$.html").toPath
    val javadoc = new File(".", "javadsl/FakerBuilderOps$.html").toPath
    val scalaOps: Map[Path, Seq[Tools.RewriteCommand]] = Map(
      key -> Seq(
        RewriteCommand(
          FileContents(key, Seq.empty),
          "fakeOp",
          FileAnchors(scaladoc, Seq("fakeOp(Int, Option):Unit"))
        )
      )
    )
    val javaOps: Map[Path, Seq[Tools.RewriteCommand]] = Map(
      key -> Seq(
        RewriteCommand(
          FileContents(key, Seq.empty),
          "fakeOp",
          FileAnchors(javadoc, Seq("fakeOp(int, Optional)"))
        )
      )
    )
    Tools.buildApidocStatements(key, scalaOps, javaOps) should be(
      """@apidoc[FakerBuilderOps.fakeOp](FakerBuilderOps$) { scala="#fakeOp(Int, Option):Unit" java="#fakeOp(int, Optional)" }"""
    )
  }

  it should "ignore the class matches when there's a match from the companion object" in {
    val key = new File(".", "fakeOp.md").toPath
    val scaladoc = new File(".", "scaladsl/FakerBuilderOps$.html").toPath
    val javadoc1 = new File(".", "javadsl/FakerBuilderOps$.html").toPath
    val javadoc2 = new File(".", "javadsl/FakerBuilderOps.html").toPath
    val scalaOps: Map[Path, Seq[Tools.RewriteCommand]] = Map(
      key -> Seq(
        RewriteCommand(
          FileContents(key, Seq.empty),
          "fakeOp",
          FileAnchors(scaladoc, Seq("fakeOp(Int, Option):Unit"))
        )
      )
    )
    val javaOps: Map[Path, Seq[Tools.RewriteCommand]] = Map(
      key -> Seq(
        RewriteCommand(
          FileContents(key, Seq.empty),
          "fakeOp",
          FileAnchors(javadoc1, Seq("fakeOp(int, Optional)"))
        ),
        RewriteCommand(
          FileContents(key, Seq.empty),
          "fakeOp",
          FileAnchors(javadoc2, Seq("fakeOp(int, Optional)"))
        )
      )
    )
    Tools.buildApidocStatements(key, scalaOps, javaOps) should be(
      """@apidoc[FakerBuilderOps.fakeOp](FakerBuilderOps$) { scala="#fakeOp(Int, Option):Unit" java="#fakeOp(int, Optional)" }"""
    )
  }

  it should "ignore the java methods that use scala types" in {
    val key = new File(".", "fakeOp.md").toPath
    val scaladoc = new File(".", "scaladsl/FakerBuilderOps$.html").toPath
    val javadoc = new File(".", "javadsl/FakerBuilderOps$.html").toPath
    val scalaOps: Map[Path, Seq[Tools.RewriteCommand]] = Map(
      key -> Seq(
        RewriteCommand(
          FileContents(key, Seq.empty),
          "fakeOp",
          FileAnchors(scaladoc, Seq("fakeOp(Int, Option):Unit"))
        )
      )
    )
    val javaOps: Map[Path, Seq[Tools.RewriteCommand]] = Map(
      key -> Seq(
        RewriteCommand(
          FileContents(key, Seq.empty),
          "fakeOp",
          FileAnchors(
            javadoc,
            Seq(
              "fakeOp(int, java.time.Period)",
              "fakeOp(int, scala.concurrent.Duration)"
            )
          )
        )
      )
    )
    Tools.buildApidocStatements(key, scalaOps, javaOps) should be(
      """@apidoc[FakerBuilderOps.fakeOp](FakerBuilderOps$) { scala="#fakeOp(Int, Option):Unit" java="#fakeOp(int, java.time.Period)" }"""
    )
  }

  it should "ignore the false positive matches (methods name contains op name)" in {
    val key = new File(".", "grouped.md").toPath
    val scaladoc = new File(".", "scaladsl/FakerBuilderOps$.html").toPath
    val scalaOps: Map[Path, Seq[Tools.RewriteCommand]] = Map(
      key -> Seq(
        RewriteCommand(
          FileContents(key, Seq.empty),
          "grouped",
          FileAnchors(
            scaladoc,
            Seq(
              "grouped[Q](Int):Unit",
              "ungrouped[Q](Int):Unit",
              "groupedMat(Int):Unit",
              "groupedWithin[T](Int):Unit"
            )
          )
        )
      )
    )
    val javaOps: Map[Path, Seq[Tools.RewriteCommand]] =
      Map.empty[Path, Seq[Tools.RewriteCommand]]
    Tools.buildApidocStatements(key, scalaOps, javaOps) should be(
      """@apidoc[FakerBuilderOps.grouped](FakerBuilderOps$) { scala="#grouped[Q](Int):Unit" }"""
    )
  }

  it should "match non-argument methods" in {
    val key = new File(".", "grouped.md").toPath
    val scaladoc = new File(".", "scaladsl/FakerBuilderOps$.html").toPath
    val scalaOps: Map[Path, Seq[Tools.RewriteCommand]] = Map(
      key -> Seq(
        RewriteCommand(
          FileContents(key, Seq.empty),
          "grouped",
          FileAnchors(scaladoc, Seq("grouped:Unit"))
        )
      )
    )
    val javaOps: Map[Path, Seq[Tools.RewriteCommand]] =
      Map.empty[Path, Seq[Tools.RewriteCommand]]
    Tools.buildApidocStatements(key, scalaOps, javaOps) should be(
      """@apidoc[FakerBuilderOps.grouped](FakerBuilderOps$) { scala="#grouped:Unit" }"""
    )
  }

  it should "generate a single @apidoc statement with java alone" in {
    val key = new File(".", "fakeOp.md").toPath
    val javadoc = new File(".", "javadsl/FakerBuilderOps.html").toPath
    val scalaOps: Map[Path, Seq[Tools.RewriteCommand]] =
      Map.empty[Path, Seq[Tools.RewriteCommand]]
    val javaOps: Map[Path, Seq[Tools.RewriteCommand]] = Map(
      key -> Seq(
        RewriteCommand(
          FileContents(key, Seq.empty),
          "fakeOp",
          FileAnchors(javadoc, Seq("fakeOp(int, Optional)"))
        )
      )
    )
    Tools.buildApidocStatements(key, scalaOps, javaOps) should be(
      """@apidoc[FakerBuilderOps.fakeOp](FakerBuilderOps) { java="#fakeOp(int, Optional)" }"""
    )
  }

  it should "generate a single @apidoc statement with scala alone" in {
    val key = new File(".", "fakeOp.md").toPath
    val scaladoc = new File(".", "scaladsl/FakerBuilderOps.html").toPath
    val scalaOps: Map[Path, Seq[Tools.RewriteCommand]] = Map(
      key -> Seq(
        RewriteCommand(
          FileContents(key, Seq.empty),
          "fakeOp",
          FileAnchors(scaladoc, Seq("fakeOp(Int, Option):Unit"))
        )
      )
    )
    val javaOps: Map[Path, Seq[Tools.RewriteCommand]] =
      Map.empty[Path, Seq[Tools.RewriteCommand]]
    Tools.buildApidocStatements(key, scalaOps, javaOps) should be(
      """@apidoc[FakerBuilderOps.fakeOp](FakerBuilderOps) { scala="#fakeOp(Int, Option):Unit" }"""
    )
  }

  it should "generate a multiple @apidoc statements with java and scala" in {
    val key = new File(".", "fakeOp.md").toPath
    val scaladoc1 = new File(".", "scaladsl/SourceFakerBuilderOps.html").toPath
    val scaladoc2 = new File(".", "scaladsl/FlowFakerBuilderOps.html").toPath
    val javadoc1 = new File(".", "javadsl/SourceFakerBuilderOps.html").toPath
    val javadoc2 = new File(".", "javadsl/FlowFakerBuilderOps.html").toPath
    val scalaOps: Map[Path, Seq[Tools.RewriteCommand]] = Map(
      key -> Seq(
        RewriteCommand(
          FileContents(key, Seq.empty),
          "fakeOp",
          FileAnchors(scaladoc1, Seq("fakeOp(Int, Option):Unit"))
        ),
        RewriteCommand(
          FileContents(key, Seq.empty),
          "fakeOp",
          FileAnchors(scaladoc2, Seq("fakeOp(Int, Option):Unit"))
        ),
      )
    )
    val javaOps: Map[Path, Seq[Tools.RewriteCommand]] = Map(
      key -> Seq(
        RewriteCommand(
          FileContents(key, Seq.empty),
          "fakeOp",
          FileAnchors(javadoc1, Seq("fakeOp(int, Optional)"))
        ),
        RewriteCommand(
          FileContents(key, Seq.empty),
          "fakeOp",
          FileAnchors(javadoc2, Seq("fakeOp(int, Optional)"))
        )
      )
    )
    Tools.buildApidocStatements(key, scalaOps, javaOps) should be(
      """@apidoc[SourceFakerBuilderOps.fakeOp](SourceFakerBuilderOps) { scala="#fakeOp(Int, Option):Unit" java="#fakeOp(int, Optional)" }
        |@apidoc[FlowFakerBuilderOps.fakeOp](FlowFakerBuilderOps) { scala="#fakeOp(Int, Option):Unit" java="#fakeOp(int, Optional)" }""".stripMargin
    )
  }

}
