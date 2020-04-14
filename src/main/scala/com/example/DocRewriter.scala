package com.example

import java.io.File
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

object DocRewriter extends App {

  implicit val system = ActorSystem()
  implicit val executionContext = system.dispatcher

  import Environment._
  import Tools._

//  private val toBeFixed: Future[Seq[FileContents]] =
//    dirContents(markdownBaseDir)
//      .filter(path => path.toFile.isFile && path.toString.endsWith(".md"))
//      .mapAsync(8)(loadContents)
//      .filter {
//        case FileContents(path, lines) =>
//          lines.exists(_.utf8String.contains("@@signature"))
//      }
//      .runWith(Sink.seq[FileContents])
//
  private val scaladslOps: Future[Seq[FileAnchors]] =
    dirContents(scaladocBaseDir)
      .filter(path => path.toFile.isFile && path.toString.endsWith(".html"))
      .mapAsync(1)(loadContents)
      .map {
        case FileContents(path, lines) =>
//          val regex = """<a id=\"""".r() //[^\\[\\(]*[\\(\\[]{1}""".r()
          val regex = """[^<]*<a id="[a-zA-Z0-9].*""".r()
          val anchors = lines
          // bytestring to string
            .map(_.utf8String)
            // keep only lines with an interesting content
            // 486:      <a id="clone():Object"></a><a id="clone():AnyRef"></a>
            .filter(regex.matches)
            .map { line =>
//              println(path + " - " + line.take(80) + "...")
              line.split("\"")(1)
            }
          FileAnchors(path, anchors)
      }
      .runWith(Sink.seq[FileAnchors])

  private val javadslOps: Future[Seq[FileAnchors]] =
    dirContents(javadocBaseDir)
      .filter(path => path.toFile.isFile && path.toString.endsWith(".html"))
      .mapAsync(1)(loadContents)
      .map {
        case FileContents(path, lines) =>
//          val regex = """<a id=\"""".r() //[^\\[\\(]*[\\(\\[]{1}""".r()
          val regex = """[^<]*<a id="[a-zA-Z0-9]*[\\(+{1].*""".r()
          val anchors = lines
          // bytestring to string
            .map(_.utf8String)
            // keep only lines with an interesting content
            // 486:      <a id="clone():Object"></a><a id="clone():AnyRef"></a>
            .filter(regex.matches)
            .map { line =>
//              println(path + " - " + line.take(80) + "...")
              line.split("\"")(1)
            }
          FileAnchors(path, anchors)
      }
      .runWith(Sink.seq[FileAnchors])

  javadslOps.onComplete(fcs => {
    fcs.map { _.map(_.methodAnchors.foreach(println)) }
    if (fcs.isFailure)
      println(s"$fcs")
    println("Done!")
    system.terminate()
  })

}

object Environment {
  val markdownBaseDir =
    "/Users/ignasi/git/github/akka/akka/unify-operator-signature-apidoc/akka-docs/src/main/paradox/stream/operators"

  val scaladocBaseDir =
    "/Users/ignasi/wip/deleteme/akka-api-docs/doc.akka.io/api/akka/2.6/akka/stream/scaladsl"

  val javadocBaseDir =
    "/Users/ignasi/wip/deleteme/akka-api-docs/doc.akka.io/japi/akka/2.6/akka/stream/javadsl"

  def dirContents(baseDir: String): Source[Path, NotUsed] =
    Directory
      .walk(new File(baseDir).toPath)
}

object Tools {
  case class FileAnchors(path: Path, methodAnchors: Seq[String])

  case class FileContents(path: Path, lines: Seq[ByteString])
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

}
