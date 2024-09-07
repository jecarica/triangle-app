package ce

import cats.effect.{IO, IOApp}
import scala.io.StdIn
import scala.util.{Try, Success, Failure}

import calculation.MinimalPath.*

object TriangleApp extends IOApp.Simple:

  // Function to read and parse triangle numbers from standard input
  private def readTriangleFromInput: IO[List[List[Int]]] = IO.println("Enter triangle numbers") >> IO {
    Iterator
      .continually(StdIn.readLine())
      .takeWhile(_ != null) // Continue until EOF (null input)
      .map(line =>
        Try(line.split("\\s+").map(_.toInt).toList) match {
          case Success(numbers) => numbers
          case Failure(_) =>
            List.empty[Int]
        }
      )
      .toList
      .filter(_.nonEmpty)
  }

  private def isValidTriangle(lists: List[List[Int]]): Boolean =
    lists.zipWithIndex.forall {
      case (innerList, index) => innerList.length == index + 1
    }


  // Main application runner
  override def run: IO[Unit] = for {
    triangle <- readTriangleFromInput
    result = if isValidTriangle(triangle) then triangle else List.empty
    _ <- IO.println(calculateMinimalPath(result))
  } yield ()


