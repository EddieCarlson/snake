package eddie

import scala.util.Random

trait Direction

object Up extends Direction
object Down extends Direction
object Left extends Direction
object Right extends Direction

case class Point(x: Int, y: Int) {
  def inDir(dir: Direction) = dir match {
    case Right => Point(x + 1, y)
    case Left => Point(x - 1, y)
    case Down => Point(x, y + 1)
    case Up => Point(x, y - 1)
  }
}

object Snake {
  def createSnake(board: Board) = {
    val headSnake = Point(board.width / 2, board.height / 2)
    val tailSnake = headSnake.inDir(Left)
    new Snake(List(headSnake, tailSnake))
  }
}

class Snake(val body: List[Point]) {
  val bodySet = body.toSet
  
  def move(dir: Direction, food: Point): Snake = {
    val newHead = body.head.inDir(dir)
    val newPoints = newHead +: (if (newHead == food) body else body.dropRight(1))
    new Snake(newPoints)
  }

  def occupies(point: Point) = {
    bodySet.contains(point)
  }
}

class Board(val width: Int, val height: Int, val food: Point) {
  val top = "_" * width
  val bot = "-" * width
  val grid = 0.to(height - 1).map { y => 0.to(width - 1).map { x => Point(x, y) } }
  val allPoints = grid.flatten.toSet

  def inBounds(p: Point) = p.x >= 0 && p.y >= 0 && p.x < width && p.y < height

  def drawPoint(p: Point, s: Snake) = {
    if (s.occupies(p))  "$"
    else if (p == food) "O"
    else                " "
  }

  def draw(snake: Snake) = {
    Seq(s" $top",
      grid.map { row => row.map { drawPoint(_, snake) }.mkString("|", "", "|") }.mkString("\n"),
      s" $bot").mkString("\n")
  }

  def placeFood(snake: Snake): Board = {
    val openPoints = allPoints -- snake.bodySet
    val foodPoint = openPoints.toSeq(Random.nextInt(openPoints.size))
    new Board(width, height, foodPoint)
  }
}



