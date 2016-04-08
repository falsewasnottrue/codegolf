import org.scalatest.{FlatSpec, Matchers}

class BracketsSpec extends FlatSpec with Matchers {

  import Brackets._

  "Brackets" should "detect truthy values" in {
    brackets("()") should be(true)
    brackets("[](){}") should be(true)
    brackets("(((())))") should be(true)
    brackets("({[<>]})") should be(true)
    brackets("[{()<>()}[]]") should be(true)
    brackets("[([]{})<{[()<()>]}()>{}]") should be(true)
  }

  it should "detect falsy values" in {
    brackets("(") should be(false)              // Has no closing ')'
    brackets("}{") should be(false)             // Wrong order
    brackets("(<)>") should be(false)           // Each pair contains only half of a matched element
    brackets("(()()foobar)") should be(false)   // Contains invalid characters
    brackets("[({}<>)>") should be(false)       // The last bracket should be ']' instead of '>'
    brackets("(((())) ") should be(false)       // Has 4 opening brackets, but only 3 closing brackets.
  }
}
