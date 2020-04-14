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

  private val toBeFixed: Future[Seq[FileContents]] = dirContents
    .filter(path => path.toFile.isFile && path.toString.endsWith(".md"))
    .mapAsync(1)(loadContents)
    .filter {
      case FileContents(path, lines) =>
        lines.exists(_.utf8String.contains("@@signature"))
    }
    .runWith(Sink.seq[FileContents])

  toBeFixed.onComplete(fcs => {
    println(s"${fcs.map { _.map(_.path) }}")
    println("Done!")
    system.terminate()
  })
}

object Environment {
  val baseDir =
    "/Users/ignasi/git/github/akka/akka/unify-operator-signature-apidoc/akka-docs/src/main/paradox/stream/operators"

  val dirContents: Source[Path, NotUsed] =
    Directory
      .walk(new File(baseDir).toPath)
}

object Tools {
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
