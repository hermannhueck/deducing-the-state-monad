package stack07

import cats.data.State

object Stack {

  type IntStack = List[Int]
  type Stack[A] = State[IntStack, A]

  def apply[A](run: IntStack => (IntStack, A)): State[IntStack, A] =
    State[IntStack, A](run)

  def empty[A]: List[A] =
    List.empty[A]

  def init(s: IntStack): Stack[Unit] =
    State.set(s)

  def reset: Stack[Unit] =
    State.set(empty[Int])

  def push(v: Int): Stack[Unit] =
    State.modify(s => v :: s)

  def pop: Stack[Int] = State { s =>
    val a1 = State.get[IntStack].map(_.head).runA(s)
    val s2 = State.modify[IntStack](_.tail).runS(s)
    (s2.value, a1.value)
  }

  def peek: Stack[Int] = view.map(_.head)

  def view: Stack[IntStack] = State.get[IntStack]
}
