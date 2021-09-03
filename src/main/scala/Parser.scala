case class Parser[T](p: String => List[(T, String)]):
  // fmap in haskell
  def map[B](f: T => B): Parser[B] = this.flatMap(x => pure(f(x)))

  // >>= in haskell
  def flatMap[B](f: T => Parser[B]): Parser[B] =
    Parser(cs => (for ((a, cs1) <- parse(this, cs)) yield parse(f(a), cs1)).flatten)

  // <> in haskell
  def +++(q: Parser[T]): Parser[T] = Parser(cs => parse(this, cs) ++ parse(q, cs))

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

@main def tests =
  val x = parse(many1(string("ha")), "hahaha")
  println(x)