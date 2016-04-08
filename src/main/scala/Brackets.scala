object Brackets extends App {

  def brackets(f: String) = bracketsAcc(Nil, f.toList)

  private def bracketsAcc(stack: List[Char], rest: List[Char]): Boolean = (stack, rest) match {
    case (Nil, Nil) => true
    case (_, Nil) => false
      
    case (s, '(' :: rs) => bracketsAcc('(' :: s, rs)
    case ('(' :: s, ')' :: rs) => bracketsAcc(s, rs)
    case (_, ')' :: rs) => false

    case (s, '[' :: rs) => bracketsAcc('[' :: s, rs)
    case ('[' :: s, ']' :: rs) => bracketsAcc(s, rs)
    case (_, ']' :: rs) => false

    case (s, '{' :: rs) => bracketsAcc('{' :: s, rs)
    case ('{' :: s, '}' :: rs) => bracketsAcc(s, rs)
    case (_, '}' :: rs) => false

    case (s, '<' :: rs) => bracketsAcc('<' :: s, rs)
    case ('<' :: s, '>' :: rs) => bracketsAcc(s, rs)
    case (_, '>' :: rs) => false

    case (_, _) => false
  }
}
