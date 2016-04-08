import org.scalatest.{FlatSpec, Matchers}

/**
  * http://codegolf.stackexchange.com/questions/77288/de-snakify-a-string
  */

/**
*Hel
  *l      rin
  *o,IAmASt g
           *S
       *!ekan
 **
 *Hello,IAmAStringSnake!
 **
 *----------

 **
 *P  ngPu  Code
*r  i  z  d  G
*o  m  z  n  o
*gram  lesA  lf
 **
 *ProgrammingPuzzlesAndCodeGolf
 **
 *----------
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

  "snakify" should "de-snakify single line" in {
    /**
      **
      *Python
      **
      *
      **
      *----------      *
      */
    val input: List[String] = "Python" :: Nil
    val expected = "Python"

    val result: String = desnakify(input)
    result should be(expected)
  }
}
