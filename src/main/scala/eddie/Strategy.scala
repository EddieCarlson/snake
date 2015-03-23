package eddie

import scala.util.Random
import scala.annotation.tailrec

trait Strategy {
  def chooseMove(snake: Snake, board: Board, food: Point): Direction
}

case class SnakePath(snake: Snake, path: Seq[Point]) {
  override def equals(other: Any) = path.head.equals(other)
  override def hashCode = path.head.hashCode
}

object RandomValidStrat extends Strategy {
  val dirs = Seq(Up, Down, Right, Left)

  def allValidUnvisitedPaths(snakePaths: Seq[SnakePath], board: Board, food: Point, remaining: Set[Point]): Seq[SnakePath] = {
    snakePaths.flatMap { snakePath =>
      val moves = validMoves(snakePath.snake, board)
      val newPaths = moves.map { case (point, dir) => SnakePath(snakePath.snake.move(dir, food), point +: snakePath.path) }
      // TODO: Direction the square was entered may also matter when attempting to visit each square?
      // TODO: necessary to unique?
      newPaths.filter { sp => remaining.contains(sp.path.head) }.toSet.toSeq
    }
  }

  // TODO: how to deal with placing new food?
  def canReachPoints(board: Board, remaining: Set[Point], snakePaths: Seq[SnakePath], food: Point, targets: Set[Point]): Boolean = {
    if (remaining.isEmpty) true
//    if (snakePaths.exists { sp => targets.forall(t => sp.path.toSet.contains(t)) }) true
    else {
      val newSnakePaths = allValidUnvisitedPaths(snakePaths, board, food, remaining)
      val newPoints = newSnakePaths.map(_.path.head)
      newSnakePaths.nonEmpty &&
        canReachPoints(board, remaining -- newPoints, snakePaths ++ newSnakePaths, food, targets)
    }
  }

  @tailrec
  def canReachPoints2(board: Board, remaining: Set[Point], snakePaths: Seq[SnakePath], food: Point, targets: Set[Point]): Seq[SnakePath] = {
    if (remaining.isEmpty) snakePaths
    //    if (snakePaths.exists { sp => targets.forall(t => sp.path.toSet.contains(t)) }) true
    else {
      val newSnakePaths = allValidUnvisitedPaths(snakePaths, board, food, remaining)
      val newPoints = newSnakePaths.map(_.path.head)
      if (newSnakePaths.nonEmpty)
        canReachPoints2(board, remaining -- newPoints, snakePaths ++ newSnakePaths, food, targets)
      else Nil
    }
  }

  @tailrec
  def explore(snake: Snake, paths: Seq[SnakePath], food: Point, board: Board, remaining: Set[Point]): Direction = {
    val foodSnake = paths.find(_.snake.body.head == food)
    foodSnake match {
      case Some(sp) if canReachPoints(board, board.allPoints, Seq(sp), food, board.allPoints) =>
//        println(sp.path)
        snake.body.head.dirTo(sp.path.last).get
      case _ =>
        val moves = allValidUnvisitedPaths(paths, board, food, remaining)
        if (moves.isEmpty) throw new Exception("cannot find valid path")
        val newPoints = moves.map(_.path.head).toSet
        println(newPoints)
        println((paths ++ moves).size)
        explore(snake, paths ++ moves, food, board, remaining -- newPoints)
    }
  }
  
  def validMoves(snake: Snake, board: Board): Map[Point, Direction] = {
    val head = snake.body.head
    val dirsToNearbyPoints = dirs.map(dir => head.inDir(dir) -> dir).toMap
    dirsToNearbyPoints.filterKeys(p => !snake.occupies(p)).filterKeys(board.inBounds)
  }

  def chooseMove(snake: Snake, board: Board, food: Point): Direction = {
    val paths = canReachPoints2(board, board.allPoints, Seq(SnakePath(snake, Nil)), food, board.allPoints)
    paths.find { sp => sp.snake.body.head == food } match {
      case Some(sp) => snake.body.head.dirTo(sp.path.last).get
      case None => paths.find { sp => sp.path.contains(food) } match {
        case Some(sp) => snake.body.head.dirTo(sp.path.last).get
        case _ => throw new Exception(s"no valid path to food at ${food}")
      }
    }
//    explore(snake, Seq(SnakePath(snake, Nil)), food, board, board.allPoints)
  }

  def chooseMoveOld(snake: Snake, board: Board, food: Point) = {
    val head = snake.body.head
    val dirsToNearbyPoints = dirs.map(dir => head.inDir(dir) -> dir).toMap
    val valid = dirsToNearbyPoints.keys.filterNot(snake.occupies).filter(board.inBounds).toSeq

    if (valid.isEmpty) throw new Exception("no valid moves!")

    val choice = valid.find(_ == food).getOrElse(valid(Random.nextInt(valid.size)))
    dirsToNearbyPoints(choice)
  }
}


