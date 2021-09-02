lazy val undefined: Nothing = throw new RuntimeException("Undefined!")

case class Queue[T](f: List[T], r: List[T], lF: Int, lR: Int):
  def isEmpty: Boolean = this.f.isEmpty
  def length: Int = lF + lR
  def snoc(v: T): Queue[T] = queue(this.f, v :: this.r, this.lF, this.lR + 1)
  def head: T = if lF == 0 then undefined else this.f.head
  def tail: Queue[T] = if lF == 0 then undefined else queue(this.f.tail, this.r, this.lF - 1, this.lR)

def queue[T](f: List[T], r: List[T], lF: Int, lR: Int): Queue[T] =
  if lF < lR then Queue(f ++ r.reverse, List(), lF + lR, 0) else Queue(f, r, lF, lR)

def empty[T] = Queue[T](List(), List(), 0, 0)

@main def main =
  val test =
    empty
      .snoc(3)
      .snoc(4)
      .snoc(5)
      .snoc(1)
      .snoc(0)
      .tail
      .tail
      .snoc(2)
  println(test)