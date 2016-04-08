object Brackets extends App {

  def brackets(f: String) = bracketsAcc(Nil, f.toList)

  private def bracketsAcc(stack: List[Char], rest: List[Char]): Boolean = (stack, rest) match {
    case (Nil, Nil) => true
    case (_, Nil) => false
    case (stack, '(' :: rs) => bracketsAcc('(' :: stack, rs)
    case ('(' :: stack, ')' :: rs) => bracketsAcc(stack, rs)
    case (_, ')' :: rs) => false

    case (stack, '[' :: rs) => bracketsAcc('[' :: stack, rs)
    case ('[' :: stack, ']' :: rs) => bracketsAcc(stack, rs)
    case (_, ']' :: rs) => false

    case (stack, '{' :: rs) => bracketsAcc('{' :: stack, rs)
    case ('{' :: stack, '}' :: rs) => bracketsAcc(stack, rs)
    case (_, '}' :: rs) => false

    case (stack, '<' :: rs) => bracketsAcc('<' :: stack, rs)
    case ('<' :: stack, '>' :: rs) => bracketsAcc(stack, rs)
    case (_, '>' :: rs) => false

    case (_, _) => false
  }
}
