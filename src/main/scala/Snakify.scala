
object Snakify {
  def desnakify(input: List[String]): String = {
    // sanity check: at least one line
    val r /*rows*/ = input.length
    val c /*columns*/ = input.head.length // sanity check: all lines have same length

    println(c)
    val sb = new StringBuilder

    var (x,y,lx,ly,end) = (0,0,-1,-1,false)
    while (!end) {
      println(x + ", " + y + ", " + lx + ", " + ly)
      sb.append(input(y)(x))

      val next = List((x+1,y),(x-1,y),(x,y+1),(x,y-1)).map(z => {println(z); z}).filter {
        case (a, b) => a >= 0 && b >= 0 && a < c && b < r && (a, b) != (lx, ly)
      }.filter {
        case (a,b) => input(b)(a) != ' '
      }

      println(next)

      if (next.length == 1) {
        lx = x
        ly = y
        x = next.head._1
        y = next.head._2
      } else {
        end = true
      }
    }
    sb.toString
  }
}
