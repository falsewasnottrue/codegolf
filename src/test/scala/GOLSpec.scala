import org.scalatest.{FlatSpec, Matchers}

class GolSpec extends FlatSpec with Matchers {

  /**
    * [[0,0,0,0],
 [0,0,0,1],
 [0,1,1,1],
 [0,0,1,0]]

[[0,0,0,0],
 [1,0,0,1],
 [1,1,0,1],
 [0,1,1,1]]

[[0,1,0,0],
 [0,1,1,1],
 [0,0,0,0],
 [0,1,0,1]]

[[0,1,0,1],
 [1,1,1,0],
 [0,1,0,1],
 [1,0,1,0]]

[[0,0,0,0],
 [0,0,0,0],
 [0,0,0,0],
 [0,0,0,0]]
    */
  val gol1 = Gol(List(
    List(0,0,0,0),
    List(0,0,0,1),
    List(0,1,1,1),
    List(0,0,1,0)
  ))


  it should "define equality" in {
    val gol = Gol(List(
      List(0,0,0,0),
      List(0,0,0,1),
      List(0,1,1,1),
      List(0,0,1,0)
    ))

    gol1.equals(gol1) shouldBe true
    gol1.equals(gol) shouldBe true

    val gol2 = Gol(List(
      List(0,1,0,0),
      List(0,0,0,1),
      List(0,1,1,1),
      List(0,0,1,0)
    ))
    gol1.equals(gol2) shouldBe false
  }

  it should "allow to access elements" in {
    gol1.get(0,0) shouldBe 0
    gol1.get(-1, 0) shouldBe 0
    gol1.get(2, 100) shouldBe 0

    gol1.get(3, 1) shouldBe 1
  }

  it should "compute neighbour count" in {
    gol1.neighbours(0,0) shouldBe 0
    gol1.neighbours(1,1) shouldBe 2
  }

  it should "compute a single step" in {
    val gol2 = gol1.next

    val expected = Gol(List(
      List(0,0,0,0),
      List(1,0,0,1),
      List(1,1,0,1),
      List(0,1,1,1)
    ))

    gol2.equals(expected) shouldBe true
  }
}