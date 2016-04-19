
/**
  * http://codegolf.stackexchange.com/questions/77745/stable-game-of-life
  */
case class Gol(elems: List[List[Int]]) {

  val cols = elems.length
  val rows = elems.head.length

  def get(col: Int, row: Int): Int = {
    if (row < 0) get(col, rows + row)
    else if (row >= rows) get(col, row - rows)
    else if (col < 0) get(cols + col, row)
    else if (col >= cols) get(col - cols, row)
    else elems(row)(col)
  }

  def neighbours(col: Int, row: Int): Int =
    get(col-1, row-1) + get(col, row-1) + get(col+1, row-1) +
    get(col-1, row) + get(col+1, row) +
    get(col-1, row+1) + get(col, row+1) + get(col+1, row+1)

  /**
    * If a cell that is off (0) is next to exactly three on (1) cells, it is turned on.
    * Otherwise, it is left off.
    * If a cell that is on is next to 2 or 3 on squares, it stays on.
    * Otherwise, it is turned off.
    */
  def next = Gol(
    elems.zipWithIndex.map {
      case (line, row) => line.zipWithIndex.map {
        case (elem, col) => (elem, (col, row))
      }
    }.map(_.map {
      case (elem, (col, row)) => (elem, neighbours(col, row))
    }).map(_.map {
      case (elem, neighbours) => (elem, neighbours) match {
        case (0, 3) => 1
        case (0, _) => 0
        case (1, 2) => 1
        case (1, 3) => 1
        case (1, _) => 0
        case _ => throw new IllegalStateException()
      }
    }))

  def stable: Boolean = next == this
}
