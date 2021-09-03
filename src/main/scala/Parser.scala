import java.lang.Character.isDigit

case class Parser[T](p: String => List[(T, String)]):
  // fmap in haskell
  def map[B](f: T => B): Parser[B] = this.flatMap(x => pure(f(x)))

  // >>= in haskell
  def flatMap[B](f: T => Parser[B]): Parser[B] =
    Parser(cs => (for ((a, cs1) <- parse(this, cs)) yield parse(f(a), cs1)).flatten)

  // <> in haskell
  def ++(q: Parser[T]): Parser[T] = Parser(cs => parse(this, cs) ++ parse(q, cs))

  // <|> in haskell
  def +++(p: Parser[T]): Parser[T] = Parser(cs => parse(this ++ p, cs) match {
    case Nil => List()
    case x :: xs => List(x)
  })

def parse[T](p: Parser[T], x: String) = p.p(x)

// pure/return in haskell
def pure[T](x: T) = Parser(cs => List((x, cs)))

def item: Parser[Char] = Parser(
  cs => cs match {
    case "" => List()
    case s => List((s.head, s.tail))
  }
)

def zero[T] = Parser[T](cs => List())

def sat(p: Char => Boolean): Parser[Char] =
  item.flatMap(c => if p(c) then pure(c) else zero)

def char(c: Char): Parser[Char] = sat(c == _)

def string(s: String): Parser[String] =
  s match {
    case "" => pure("")
    case s => {
      for
        _ <- char(s.head)
        _ <- string(s.tail)
      yield
        s
    }
  }

def many[T](p: Parser[T]): Parser[List[T]] =
  many1(p) +++ pure(List())

def many1[T](p: Parser[T]): Parser[List[T]] =
  for
    a <- p
    as <- many(p)
  yield
    a :: as

def sepby[A, B](p: Parser[A], sep: Parser[B]): Parser[List[A]] =
  sepby1(p, sep) +++ pure(List())

def sepby1[A, B](p: Parser[A], sep: Parser[B]): Parser[List[A]] =
  for
    a <- p
    as <- many(sep.flatMap(_ => p))
  yield
    a :: as

def chainl[T](p: Parser[T], op: Parser[(T, T) => T], a: T): Parser[T] =
  chainl1(p, op) +++ pure(a)

def chainl1[T](p: Parser[T], op: Parser[(T, T) => T]): Parser[T] =
  lazy val rest: T => Parser[T] = (a: T) => {
    for
      f <- op
      b <- p
      result <- rest(f(a, b))
    yield
      result
  } +++ pure(a)
  p.flatMap(a => rest(a))

def space: Parser[String] = many(sat(_ == ' ')).map(_.mkString)

def token[T](p: Parser[T]): Parser[T] =
  for
    a <- p
    _ <- space
  yield
    a

def symb(cs: String): Parser[String] = token(string(cs))

def apply[T](p: Parser[T], x: String): List[(T, String)] = parse(space.flatMap(_ => p), x)

lazy val expr: Parser[Int] = chainl1(term, addop)
lazy val term = chainl1(factor, mulop)
lazy val factor = digit +++ {
  for
    _ <- symb("(")
    n <- expr
    _ <- symb(")")
  yield
    n
}
lazy val digit = {
  for
    x <- token(sat(isDigit))
  yield
    x - '0'
}
lazy val addop = symb("+").flatMap(_ => pure((_: Int) + (_: Int))) +++ symb("-").flatMap(_ => pure((_: Int) - (_: Int)))
lazy val mulop = symb("*").flatMap(_ => pure((_: Int) * (_: Int))) +++ symb("/").flatMap(_ => pure((_: Int) / (_: Int)))

@main def tests =
  // does not work :(
  val x = apply(expr, " 1 - 2 * 3 + 4 ")
  println(x)