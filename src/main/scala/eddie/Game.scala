package eddie

object Game extends App {
  val board = new Board(4, 4, Point(3, 3))
  val snake = Snake.createSnake(board)

  def play(s: Snake, b: Board): Unit = {
    println(b.draw(s))
    val move = RandomValidStrat.chooseMove(s, b)
    val newSnake = s.move(move, b.food)
    val newBoard = if (newSnake.body.head == b.food) b.placeFood(newSnake) else b
    Thread.sleep(500L)
    play(newSnake, newBoard)
  }

  play(snake, board)
}

