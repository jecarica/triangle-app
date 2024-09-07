package calculation


object MinimalPath:
  /*** Starting from the nodes on the bottom row we get the path with min sum for these nodes as the values of the nodes themselves.
    After the initialization, path with minimal sum at the ith node of the corresponding row is the minimum of the minimal path sums of
    its two children + the node’s value.
   ***/

  final case class Calculation(minimalPath: List[Int], sum: Int)

  final case class CurrentNodeCalculation(node: Int, leftChildCalculation: Calculation, rightChildCalculation: Calculation)

  private def currentRowCalculation(upperRow: List[Int], bottomRow: List[Calculation]): List[Calculation] =

    upperRow.zip(bottomRow.zip(bottomRow.tail)).map {
      case currentNode=>
        val nodeCalculation = CurrentNodeCalculation(node = currentNode._1, leftChildCalculation = currentNode._2._1, rightChildCalculation = currentNode._2._2)
        val minChild =
          if nodeCalculation.rightChildCalculation.sum < nodeCalculation.leftChildCalculation.sum then nodeCalculation.rightChildCalculation
          else nodeCalculation.leftChildCalculation
        Calculation(nodeCalculation.node :: minChild.minimalPath, nodeCalculation.node + minChild.sum)
    }

  def calculateMinimalPath(triangleNumbers: List[List[Int]]): String =
    if triangleNumbers.isEmpty then  ""
    else
      val initialCalculations =
        List.fill[Calculation](triangleNumbers.last.length + 1)(Calculation(List(), 0))

      val result =
        triangleNumbers.foldRight(initialCalculations)(currentRowCalculation).head

      s"Minimal path is: ${result.minimalPath.mkString(" + ")} = ${result.sum}"

