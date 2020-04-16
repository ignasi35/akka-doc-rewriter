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
    val scaldoc = new File(".", "FakerBuilderOps.html").toPath
    val javadoc = new File(".", "FakerBuilderOps.html").toPath
    val scalaOps: Map[Path, Seq[Tools.RewriteCommand]] = Map(
      key -> Seq(
        RewriteCommand(
          FileContents(key, Seq.empty),
          "fakeOp",
          FileAnchors(scaldoc, Seq("fakeOp(Int, Option):Unit"))
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

}
