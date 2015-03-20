package eddie

import scala.util.Random

trait Direction

object Up extends Direction
object Down extends Direction
object Left extends Direction
object Right extends Direction

trait Square

object Free extends Square { override def toString = " " }
object Food extends Square { override def toString = "O" }
case class Snake(next: Option[Direction]) extends Square {
  override def toString = next match {
    case None => "X"
    case Some(Up) => "^"
    case Some(Down)=> "v"
    case Some(Left) => "<"
    case Some(Right) => ">"
  }
}

trait Pointy
case class Point(x: Int, y: Int) extends Pointy
case object NoPoint extends Pointy

object Board {
  def initial(x: Int, y: Int) = {
    val head = Point(5, 5)
    val tail = Point(4, 5)
    val grid: Seq[Seq[Square]] =
      0.to(x - 1).map { yy => 0.to(y - 1).map { xx =>
        if (xx == 5 && yy == 5) Snake(None)
        else if (xx == 4 && yy == 5) Snake(Some(Right))
        else if (xx == 8 && yy == 5) Food
        else Free
      } }

    new Board(new Grid(grid), head, tail)
  }
}

class Grid(val grid: Seq[Seq[Square]]) {
  def apply(point: Point) = grid(point.y)(point.x)
}

class Board(grid: Grid, head: Point, tail: Point) {
  def newHead(dir: Direction): Point = pointInDir(head, dir)
  def newTail = pointInDir(tail, getTail.next.get)

  def pointInDir(point: Point, dir: Direction) = dir match {
    case Right => Point(point.x + 1, point.y)
    case Left => Point(point.x - 1, point.y)
    case Down => Point(point.x, point.y + 1)
    case Up => Point(point.x, point.y - 1)
  }

  def changeSquare(x: Int, y: Int, newHeadPoint: Point, dir: Direction, grow: Boolean, newFoodPoint: Pointy): Square = {
    Point(x, y) match {
      case `newHeadPoint` => Snake(None)
      case `head` => Snake(Some(dir))
      case `tail` if (!grow) => Free
      case `newFoodPoint` => Food
      case p => grid(p)
    }
  }

  def getTail: Snake = grid(tail) match {
    case s: Snake => s
    case _ => throw new Exception("getTail should have been a snake")
  }

  override def toString = {
    val top = "_" * grid.grid(0).size
    val bot = "-" * grid.grid(0).size
    Seq(s" $top",
      grid.grid.map { row => row.map { sq => sq.toString }.mkString("|", "", "|") }.mkString("\n"),
      s" $bot").mkString("\n")
  }

  def isOutOfBounds(point: Point) =
    if (point.x < 0 || point.y < 0 || point.x >= grid.grid.size || point.y >= grid.grid(0).size)
      throw new Exception(s"move out of bounds: $point")

  def isSnakePoint(point: Point) = {
    grid(point) match {
      case s: Snake => throw new Exception(s"moving into snake: $point")
      case _ => Unit
    }
  }

  def checkLegality(newH: Point) = {
    isOutOfBounds(newH)
    isSnakePoint(newH)
  }

  def findNewFoodPoint: Point = {
    val available = grid.grid.zip(0.to(grid.grid.size - 1)).map {
      case (row, y) => row.zip(0.to(row.size - 1)).map {
        case (sq, x) =>
          val p = Point(x, y)
          grid(p) match {
            case Free => Some(p)
            case _ => None
          }
      }.flatten
    }.flatten

    available(Random.nextInt(available.size - 1))
  }

  def move(dir: Direction): Board = {
    //    println(s"head: $head")
    //    println(s"tail: $tail")
    val newHeadPoint = newHead(dir)
    checkLegality(newHeadPoint)
    val eats = grid(newHeadPoint) match {
      case Food => true
      case _ => false
    }
    val newFoodPoint = if (eats) findNewFoodPoint else NoPoint
    val newGrid = grid.grid.zip(0.to(grid.grid.size - 1)).map {
      case (row, y) => row.zip(0.to(row.size - 1)).map {
        case (sq, x) => changeSquare(x, y, newHeadPoint, dir, eats, newFoodPoint)
      }
    }
    new Board(new Grid(newGrid), newHeadPoint, if (eats) tail else newTail)
  }
}
