package com.example

import java.io.File
import java.nio.file.Path
import java.nio.file.Paths

import com.example.Tools.FileAnchors
import com.example.Tools.FileContents
import com.example.Tools.RewriteCommand
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class DocRewriterSpec extends FlatSpec with Matchers {
  behavior of "Apidoc statment builder"

  it should "generate a single @apidoc statement with java and scala" in {
    val key = new File(".", "fakeOp.md").toPath
    val scaladoc = new File(".", "FakerBuilderOps.html").toPath
    val javadoc = new File(".", "FakerBuilderOps.html").toPath
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
      """@apidoc[fakeOp](FakerBuilderOps) { scala="#fakeOp(Int, Option):Unit" java="#fakeOp(int, Optional)" }"""
    )
  }

  it should "generate a single @apidoc statement with java alone" in {
    val key = new File(".", "fakeOp.md").toPath
    val javadoc = new File(".", "FakerBuilderOps.html").toPath
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
      """@apidoc[fakeOp](FakerBuilderOps) { java="#fakeOp(int, Optional)" }"""
    )
  }

  it should "generate a single @apidoc statement with scala alone" in {
    val key = new File(".", "fakeOp.md").toPath
    val scaladoc = new File(".", "FakerBuilderOps.html").toPath
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
      """@apidoc[fakeOp](FakerBuilderOps) { scala="#fakeOp(Int, Option):Unit" }"""
    )
  }

  it should "generate a multiple @apidoc statements with java and scala" in {
    val key = new File(".", "fakeOp.md").toPath
    val scaladoc1 = new File(".", "SourceFakerBuilderOps.html").toPath
    val scaladoc2 = new File(".", "FlowFakerBuilderOps.html").toPath
    val javadoc1 = new File(".", "SourceFakerBuilderOps.html").toPath
    val javadoc2 = new File(".", "FlowFakerBuilderOps.html").toPath
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
      """@apidoc[fakeOp](SourceFakerBuilderOps) { scala="#fakeOp(Int, Option):Unit" java="#fakeOp(int, Optional)" }
        |@apidoc[fakeOp](FlowFakerBuilderOps) { scala="#fakeOp(Int, Option):Unit" java="#fakeOp(int, Optional)" }""".stripMargin
    )
  }

}
