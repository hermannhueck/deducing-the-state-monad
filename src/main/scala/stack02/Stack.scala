package stack02

object Stack {

  type Stack[A] = List[A]

  def apply[A](as: A*): Stack[A] =
    List.apply(as: _*)

  def empty[A]: Stack[A] =
    List.empty[A]

  def init(s: Stack[Int]): (Stack[Int], Unit) =
    (s, ())

  def reset: (Stack[Int], Unit) = init(empty[Int])

  def push(s: Stack[Int], v: Int): (Stack[Int], Unit) =
    (v :: s, ())

  def pop(s: Stack[Int]): (Stack[Int], Int) =
    (s.tail, s.head)

  def peek(s: Stack[Int]): (Stack[Int], Int) =
    (s, s.head)

  def view(s: Stack[Int]): (Stack[Int], Stack[Int]) =
    (s, s)
}
