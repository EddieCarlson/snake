package eddie

object Game extends App {
  val initialBoard = Board.initial(10, 10)
  var board = initialBoard
  println(board)
  val dirs = Seq(Right, Right, Right, Right, Down, Down, Down, Down, Left, Left, Left, Left, Up, Up, Up, Up)
  1.to(7).foreach { i =>
    dirs.foreach { dir =>
      Thread.sleep(200L)
      board = board.move(dir)
      println(board)
    }
  }
}
