import org.scalatest.{FlatSpec, Matchers}

/**
  * http://codegolf.stackexchange.com/questions/77288/de-snakify-a-string
  */

/**

 **
 *~ zyx tsr XWVUTSR
   *}|{ wvu q Y     Q
 *
*!          p Z `ab P
*"#$ 6789:; o [ _ c O
  *% 5    < n \]^ d N
*('& 432  = m     e M
*)     1  > lkjihgf L
  *+,-./0  ?         K
         *@ABCDEFGHIJ
 **
 *!"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
 **
  * ----------
  **/

import Snakify._

class SnakifySpecs extends FlatSpec with Matchers {

  "desnakify" should "work with single line" in {
    val input: List[String] = "Python" :: Nil
    val expected = "Python"

    val result: String = desnakify(input)
    result should be(expected)
  }

  it should "work with multiple lines" in {
    val input: List[String] =
      "Hel         " ::
      "  l      rin" ::
      "  o,IAmASt g" ::
      "           S" ::
      "       !ekan" :: Nil
    val expected = "Hello,IAmAStringSnake!"

    val result: String = desnakify(input)
    result should be(expected)
  }

  it should "work 2" in {
    val input: List[String] =
      "P  ngPu  Code " ::
      "r  i  z  d  G " ::
      "o  m  z  n  o " ::
      "gram  lesA  lf" :: Nil
    val expected = "ProgrammingPuzzlesAndCodeGolf"

    val result: String = desnakify(input)
    result should be(expected)
  }
}
