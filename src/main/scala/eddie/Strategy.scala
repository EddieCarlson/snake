package eddie

import scala.util.Random

trait Strategy {
  def chooseMove(snake: Snake, board: Board): Direction
}

object RandomValidStrat extends Strategy {
  val dirs = Seq(Up, Down, Right, Left)

  def chooseMove(snake: Snake, board: Board) = {
    val head = snake.body.head
    val dirsToNearbyPoints = dirs.map(dir => head.inDir(dir) -> dir).toMap
    val valid = dirsToNearbyPoints.keys.filterNot(snake.occupies).filter(board.inBounds).toSeq

    if (valid.isEmpty) throw new Exception("no valid moves!")

    val choice = valid.find(_ == board.food).getOrElse(valid(Random.nextInt(valid.size)))
    dirsToNearbyPoints(choice)
  }
}


