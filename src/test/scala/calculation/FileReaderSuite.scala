package calculation

import helper.FileReader
import munit.CatsEffectSuite

class FileReaderSuite extends CatsEffectSuite {

  test("read numbers in suitable list format from file") {
    val file = FileReader("sample.txt")
    file.readWithResource.map(it => assertEquals(it, List(List(1), List(2, 3))))
  }
}
