object ScalaNQueens extends App {
  type Board = List[Int]

  override def main(args: Array[String]): Unit = {
    val dimensions: Int = RequireDimensions(args)
    AttemptSolution(List[Int](), dimensions)
  }

  private def AskForDimensions(): Int = {
    println("Please enter a dimension...")
    scala.io.StdIn.readInt()
  }

  private def RequireDimensions(args: Array[String]): Int = {
    args.length match {
      case 0 => AskForDimensions()
      case _ => args(0).toInt
    }
  }

  // I believe it is possible to do this with a foldLeft, to try another day...
  private def AttemptSolution(board: Board, dimensions: Int): Unit = {
    board.length match {
      case `dimensions` => PrintSolution(board)
      case _ =>
        (0 until dimensions)
        .foreach((position: Int) => {
          if(!board.contains(position)) {
            val updatedBoard: Board = board :+ position
            if(OkaySoFar(updatedBoard)) {
              AttemptSolution(updatedBoard, dimensions)
            }
          }
        })
    }
  }

  private def OkaySoFar(board: Board): Boolean = {
    val size: Int = board.length - 1
    !((0 until size) exists { index: Int =>  size - index == Math.abs(board(size) - board(index)) })
  }

  private def PrintSolution(board: Board): Unit = {
    print("[ ")
    board foreach { position: Int => print(s"$position ") }
    println(" ]")
  }
}
