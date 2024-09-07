package calculation

import cats.effect.IO
import cats.effect.testing.specs2.CatsEffect
import helper.FileReader
import org.specs2.mutable.Specification

class MinimalPathSpecSpec extends Specification with CatsEffect with Fixtures:

  def program(file: FileReader) =
    for
      content <- file.readWithResource
      result = MinimalPath.calculateMinimalPath(content)
    yield result

  "program" should {
    "return correct result" in {
      val file = FileReader("data_small.txt")
      val result = program(file)
      result.map(_ === dataSmallResult)
    }

    "return correct result" in {
      val file = FileReader("data_big.txt")
      val result = program(file)
      result.map(_ === dataBigResult)
    }
  }