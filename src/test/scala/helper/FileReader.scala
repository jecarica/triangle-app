package helper

import cats.effect.{IO, Resource}

import scala.io.Source
import cats.syntax.all.*

case class FileReader(fileName: String):
  def sourceIO: IO[Source] =
    IO.println("Acquiring source to read file") >> IO(
      Source.fromResource(fileName)
    )

  def closeFile(source: Source): IO[Unit] =
    IO.println("Closing the file after read") >> IO(source.close())
    
  val makeResourceForRead: Resource[IO, Source] =
    Resource.make(sourceIO)(src => closeFile(src))
  val readWithResource =
    makeResourceForRead.use(src => readLines(src))  

  def readLines(source: Source) =
    IO.println("Reading contents from the file") >> IO(
      source.getLines().map(line => 
        line.split(" ")
        .map(_.toInt)
        .toList
      ).toList)
