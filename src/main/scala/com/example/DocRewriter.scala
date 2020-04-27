package com.example

import java.io.File
import java.io.FileOutputStream
import java.io.PrintWriter
import java.nio.file.Path

import akka.Done
import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.ActorAttributes
import akka.stream.alpakka.file.scaladsl.Directory
import akka.stream.scaladsl.FileIO
import akka.stream.scaladsl.Framing
import akka.stream.scaladsl.Sink
import akka.stream.scaladsl.Source
import akka.util.ByteString

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.matching.Regex

object DocRewriter extends App {

  implicit val system = ActorSystem()
  implicit val executionContext = system.dispatcher

  import Environment._
  import Tools._

  private val fMarkdownTargets: Future[Seq[FileContents]] =
    dirContents(markdownBaseDir)
      .filter(path => path.toFile.isFile && path.toString.endsWith(".md"))
      .mapAsync(8)(loadContents)
      .filter {
        case FileContents(path, lines) =>
          lines.exists(_.utf8String.contains("@@signature"))
      }
      .runWith(Sink.seq[FileContents])

  private val fScaladslOps: Future[Seq[FileAnchors]] =
    loadMethodAnchors(scaladocBaseDir, """[^<]*<a id="[a-zA-Z0-9].*""".r())

  private val fJavadslOps: Future[Seq[FileAnchors]] =
    loadMethodAnchors(
      javadocBaseDir,
      """[^<]*<a id="[a-zA-Z0-9]*[\\(+{1].*""".r()
    )

  val eventually: Future[Done] =
    for {
      markdownTargets: Seq[FileContents] <- fMarkdownTargets
      scaladslOps: Seq[FileAnchors] <- fScaladslOps
      javadslOps: Seq[FileAnchors] <- fJavadslOps
    } yield {

      val scalaRewriteOps: Map[Path, Seq[RewriteCommand]] =
        buildRewriteOps(markdownTargets, scaladslOps)
      val javaRewriteOps: Map[Path, Seq[RewriteCommand]] =
        buildRewriteOps(markdownTargets, javadslOps)

      for {
        key <- scalaRewriteOps.keySet ++ javaRewriteOps.keySet
      } yield {

        val targetMap =
          markdownTargets.map(md => (md.path, md)).toMap

        val apidocStatements =
          buildApidocStatements(key, scalaRewriteOps, javaRewriteOps)
        val newContent = {
          targetMap(key).lines
            .map(_.utf8String)
            .filterNot(_.contains("@@signature"))
            .mkString("\n")
            .replace("""|## Signature
                        |
                        |""".stripMargin, s"""## Signature
                |
                |$apidocStatements""".stripMargin)
        }

        val printWriter = new PrintWriter(
          new FileOutputStream(key.toFile.getAbsolutePath)
        )
        try {
          printWriter.write(newContent)
        } finally {
          printWriter.close()
        }
      }

      Done
    }

  eventually.onComplete(fcs => {
    if (fcs.isFailure)
      println(s"$fcs")
    println("Done!")
    system.terminate()
  })

}

object Tools {
  case class RewriteCommand(target: FileContents,
                            op: String,
                            fileAnchor: FileAnchors)

  case class FileAnchors(path: Path, methodAnchors: Seq[String])

  case class FileContents(path: Path, lines: Seq[ByteString])

  def dirContents(baseDir: String): Source[Path, NotUsed] =
    Directory
      .walk(new File(baseDir).toPath)

  def opName(path: Path): String = path.getFileName.toString.replace(".md", "")

  // given a Path, return a tuple of the path and the file contents loaded as a sequence of file lines
  def loadContents(path: Path)(implicit exCtx: ExecutionContext,
                               system: ActorSystem): Future[FileContents] = {
    val fileContentsSource = FileIO
      .fromPath(path)
      .withAttributes(
        ActorAttributes.dispatcher("custom-blocking-io-dispatcher")
      )
      .via(
        Framing.delimiter(
          ByteString("\n"),
          maximumFrameLength = Int.MaxValue,
          allowTruncation = true
        )
      )
    fileContentsSource
      .runWith(Sink.seq)
      .map { lines =>
        FileContents(path, lines)
      }
  }

  def loadMethodAnchors(
    baseDir: String,
    regex: Regex
  )(implicit exCtx: ExecutionContext, system: ActorSystem) = {
    dirContents(baseDir)
      .filter(path => path.toFile.isFile && path.toString.endsWith(".html"))
      .mapAsync(1)(loadContents)
      .map {
        case FileContents(path, lines) =>
          val anchors = lines
            .map(_.utf8String)
            .filter(regex.matches)
            .map { line =>
              line.split("\"")(1)
            }
          FileAnchors(path, anchors)
      }
      .runWith(Sink.seq[FileAnchors])

  }

  def buildRewriteOps(
    markdownTargets: Seq[FileContents],
    apidocsAnchors: Seq[FileAnchors]
  ): Map[Path, Seq[RewriteCommand]] = {
    markdownTargets
      .flatMap { markdownTarget =>
        {
          val op = opName(markdownTarget.path)
          apidocsAnchors
            .map { scaladslOp =>
              scaladslOp.copy(
                methodAnchors = scaladslOp.methodAnchors.filter(_.contains(op))
              )
            }
            .filter(_.methodAnchors.nonEmpty)
            .map(fileAnchor => RewriteCommand(markdownTarget, op, fileAnchor))
        }
      }
      .groupBy(_.target.path)
      .toMap
  }

  def buildApidocStatements(
    key: Path,
    scalaRewriteOps: Map[Path, Seq[RewriteCommand]],
    javaRewriteOps: Map[Path, Seq[RewriteCommand]]
  ): String = {
    val operatorName = scalaRewriteOps
      .get(key)
      .map(_.head.op)
      .getOrElse(javaRewriteOps.get(key).map(_.head.op).get)

    def flattenAnchors(input: Map[Path, Seq[RewriteCommand]],
                       dsl: String): Iterable[(String, String)] = {
      input
        .get(key)
        .toSeq
        .flatMap { rewriteCommands =>
          rewriteCommands.flatMap { rewriteCommand =>
            rewriteCommand.fileAnchor.methodAnchors.map { methodAnchor =>
              val typeName =
                rewriteCommand.fileAnchor.path.getFileName.toString
                  .replace(".html", "")
              (typeName, s"""${dsl}="#${methodAnchor}" """)
            }
          }
        }
    }
    val anchors: Iterable[(String, String)] = flattenAnchors(
      scalaRewriteOps,
      "scala"
    ) ++ flattenAnchors(javaRewriteOps, "java")

    anchors
      .groupBy(_._1)
      .map {
        case (typeName, anchors) =>
          s"""@apidoc[$operatorName]($typeName) { ${anchors
               .map(_._2)
               .mkString("")}}""".stripMargin
      }
      .mkString("\n")

  }

}

object Environment {
  val markdownBaseDir =
    "/Users/ignasi/git/github/akka/akka/unify-operator-signature-apidoc/akka-docs/src/main/paradox/stream/operators"

  val scaladocBaseDir =
    "/Users/ignasi/wip/deleteme/akka-api-docs/doc.akka.io/api/akka/2.6/akka/stream/scaladsl"

  val javadocBaseDir =
    "/Users/ignasi/wip/deleteme/akka-api-docs/doc.akka.io/japi/akka/2.6/akka/stream/javadsl"

}
