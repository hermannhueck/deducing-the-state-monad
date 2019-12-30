package stack01

object Stack {

  type Stack[A] = List[A]

  def apply[A](as: A*): Stack[A] = List.apply(as: _*)

  def empty[A]: Stack[A] = List.empty[A]

  var stack: Stack[Int] = empty[Int]

  def init(s: Stack[Int]): Unit =
    stack = s

  def reset(): Unit =
    init(empty[Int])

  def push(v: Int): Unit =
    stack = v :: stack

  def pop: Int = {
    val h = stack.head
    stack = stack.tail
    h
  }

  def peek: Int = stack.head

  def view: Stack[Int] = stack
}
