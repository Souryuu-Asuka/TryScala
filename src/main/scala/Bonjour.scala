enum Color:
  case Red
  case Black

class Either[T, R]

case class Left[T, R](value: T) extends Either[T, R]
case class Right[T, R](value: R) extends Either[T, R]

trait Show[T]:
  def show(a: T): Unit

def qsort(xs: List[Int]): List[Int] =
  xs match {
    case Nil => Nil
    case p :: ps => qsort(ps.filter(_ <= p)) ++ List(p) ++ qsort(ps.filter(_ > p))
  }

@main def Bonjour(): Unit =
  println("Bonjour")
  println(Color.Red)
  val x = Left(233)
  println(x)
