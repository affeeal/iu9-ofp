// the first way
val attachHead: (Int, List[List[Int]]) => List[List[Int]] = {
  case (x, Nil) => Nil
  case (x, l :: ls) => (x :: l) :: attachHead(x, ls)
}

val comb1: Int => (List[Int] => List[List[Int]]) =
  k => {
    case xs if (k == 0) => List(List())
    case Nil => Nil
    case x :: xs => attachHead(x, comb1(k - 1)(xs)) ::: comb1(k)(xs)
  }

// the second way
val comb2: Int => (List[Int] => List[List[Int]]) =
  k => {
    def combInner: (List[Int], Int, List[Int]) => List[List[Int]] = {
      case (xs, k, s) if (xs.length < k) => Nil
      case (xs, k, s) if (k == 0) => List(s)
      case (x :: xs, k, s) => combInner(xs, k, s) ::: combInner(xs, k - 1, x :: s)
    }

    combInner(_: List[Int], k, List())
  }
