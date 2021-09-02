def slice[T](xs: List[T], i: Int, j: Int): List[T] =
  (xs, i, j) match {
    case (Nil, _, _) => Nil
    case (_, 0, 0) => Nil
    case (h :: t, 0, j) => h :: slice(t, 0, j - 1)
    case (h :: t, i, j) => slice(t, i - 1, j - 1)
  }

enum Nat:
  case Zero
  case Succ(n: Nat)

  def toInt: Int =
    this match {
      case Zero => 0
      case Succ(n) => 1 + n.toInt
    }

def add(a: Nat, b: Nat): Nat =
  (a, b) match {
    case (Nat.Zero, m) => m
    case (Nat.Succ(n), m) => add(n, Nat.Succ(m))
  }

object Hello extends App {
  println(slice(List(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 6))
  println(Nat.Succ(Nat.Succ(Nat.Zero)).toInt)
  println(add(Nat.Succ(Nat.Succ(Nat.Zero)), Nat.Succ(Nat.Succ(Nat.Zero))).toInt)
}
