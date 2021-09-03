val x: Int =
  List.range(1, 20)
    .zip(List.range(20, 40).reverse)
    .map((x, y) => x * y)
    .filter(_ % 2 == 0)
    .map(_ + 1)
    .foldRight(0)(_ + _)

def liftAdd(a: Option[Int], b: Option[Int]): Option[Int] =
  for
    x <- a
    y <- b
  yield
    x + y

def product(a: List[Int], b: List[Int]): List[(Int, Int)] =
  for
    x <- a
    y <- b
  yield
    (x, y)

trait Showable[A]:
  extension(a: A)
    def show: String
    def showIO(): Unit

case class Box(value: String, len: Int)

given Showable[Box] with
  extension(b: Box)
    def showIO(): Unit =
      for _ <- 0 to b.len do
        println(b.value)
    def show: String =
      b.value ++ s"${b.len}"

@main def Haha(): Unit =
  for x <- 1 to 10 do
    println(x)

  println(x)
  println(liftAdd(Some(3), None))
  println(Box("hello", 3).show)
  Box("bonjour", 5).showIO()
  println(product(List(1,2,3), List(4,5,6)))
