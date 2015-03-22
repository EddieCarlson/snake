package eddie

import scala.annotation.tailrec

object Game extends App {
  val board = new Board(4, 4)
  val snake = Snake.createSnake(board)
  val food = board.randomOpen(snake)

  @tailrec
  def play(s: Snake, b: Board, f: Point): Unit = {
    println(Draw.draw(s, f, b))
    val move = RandomValidStrat.chooseMove(s, b, f)
    val newSnake = s.move(move, f)
    val newFood = if (newSnake.body.head == f) b.randomOpen(newSnake) else f
    Thread.sleep(500L)
    play(newSnake, b, newFood)
  }

  play(snake, board, food)
}

