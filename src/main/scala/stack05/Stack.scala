package stack05

object Stack {

  type IntStack = List[Int]
  type Stack[A] = State[IntStack, A]

  def apply[A](run: IntStack => (IntStack, A)): State[IntStack, A] =
    State[IntStack, A](run)

  def empty[A]: List[A] = List.empty[A]

  def init(s: IntStack): Stack[Unit] = Stack { _ =>
    (s, ())
  }

  def reset: Stack[Unit] = Stack { _ =>
    (empty[Int], ())
  }

  def push(v: Int): Stack[Unit] = Stack { s =>
    (v :: s, ())
  }

  def pop: Stack[Int] = Stack { s =>
    (s.tail, s.head)
  }

  def peek: Stack[Int] = Stack { s =>
    (s, s.head)
  }

  def view: Stack[IntStack] = Stack { s =>
    (s, s)
  }
}
