package eddie

import scala.util.Random

sealed trait Direction

object Up extends Direction
object Down extends Direction
object Left extends Direction
object Right extends Direction

case class Point(x: Int, y: Int) {
  def inDir(dir: Direction) = dir match {
    case Up =>    Point(x, y - 1)
    case Down =>  Point(x, y + 1)
    case Right => Point(x + 1, y)
    case Left =>  Point(x - 1, y)
  }

  def dirTo(p: Point) = Seq(Up, Down, Left, Right).find(inDir(_) == p)

  override def equals(other: Any) = other match {
    case Point(ox, oy) => x == ox && y == oy
    case _ => false
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

object Draw {
  private def drawPoint(p: Point, s: Snake, food: Point) = {
    if (s.occupies(p))  "$"
    else if (p == food) "O"
    else                " "
  }

  def draw(snake: Snake, food: Point, board: Board) = {
    Seq(s" ${board.topBar}",
      board.grid.map { row => row.map { drawPoint(_, snake, food) }.mkString("|", "", "|") }.mkString("\n"),
      s" ${board.botBar}").mkString("\n")
  }
}

class Board(val width: Int, val height: Int) {
  val topBar = "_" * width
  val botBar = "-" * width

  val grid = 0.to(height - 1).map { y => 0.to(width - 1).map { x => Point(x, y) } }
  val allPoints = grid.flatten.toSet

  def inBounds(p: Point) = p.x >= 0 && p.y >= 0 && p.x < width && p.y < height

  def randomOpen(snake: Snake): Point = {
    val openPoints = allPoints -- snake.bodySet
    openPoints.toSeq(Random.nextInt(openPoints.size))
  }
}



