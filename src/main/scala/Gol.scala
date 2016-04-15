
/**
  * http://codegolf.stackexchange.com/questions/77745/stable-game-of-life
  */
case class Gol(elems: List[List[Int]]) {

  val cols = elems.length
  val rows = elems.head.length

  def get(col: Int, row: Int) = {
    if (row < 0 || row >= rows || col < 0 || col > cols) {
      0
    } else {
      elems(row)(col)
    }
  }

  def neighbours(col: Int, row: Int): Int =
    get(col-1, row-1) + get(col, row-1) + get(col+1, row-1) +
    get(col-1, row) + get(col, row) + get(col+1, row) +
    get(col-1, row+1) + get(col, row+1) + get(col+1, row+1)

  /**
    * If a cell that is off (0) is next to exactly three on (1) cells, it is turned on.
    * Otherwise, it is left off.
    * If a cell that is on is next to 2 or 3 on squares, it stays on.
    * Otherwise, it is turned off.
    */
  def next: Gol = {
    val withPos = elems.zipWithIndex.map {
      case (line, row) => line.zipWithIndex.map {
        case (elem, col) => (elem, (row, col))
      }
    }
    println(withPos)

    val withNeighbors = withPos.map(_.map {
      case (elem, (row, col)) => (elem, neighbours(col, row))
    })
    println(withNeighbors)

    val next = withNeighbors.map(_.map {
      case (elem, neighbours) => (elem, neighbours) match {
        case (0, 3) => 1
        case (0, _) => 0
        case (1, 2) => 1
        case (1, 3) => 1
        case (1, _) => 0
        case _ => throw new IllegalStateException()
      }
    })

    Gol(next)
  }
}
