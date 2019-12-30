package stack03

object Stack {

  type Stack[A] = List[Int] => (List[Int], A)

  def empty[A]: List[A] = List.empty[A]

  val init: Stack[Unit] =
    s => (s, ())

  val reset: Stack[Unit] =
    _ => init(empty[Int])

  def push(v: Int): Stack[Unit] =
    s => (v :: s, ())

  val pop: Stack[Int] =
    s => (s.tail, s.head)

  val peek: Stack[Int] =
    s => (s, s.head)

  val view: Stack[List[Int]] =
    s => (s, s)
}
