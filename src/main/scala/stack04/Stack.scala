package stack04

import cats.Monad

case class Stack[A](run: List[Int] => (List[Int], A)) {

  def runS: List[Int] => List[Int] =
    li => run(li)._1

  def runA: List[Int] => A =
    li => run(li)._2

  // map doesn't manipulate the stack, it just transforms the A value
  def map[B](f: A => B): Stack[B] = Stack { s =>
    val (s1, a1) = run(s)
    (s1, f(a1))
  }

  def flatMap[B](f: A => Stack[B]): Stack[B] = Stack { s =>
    val (s1, a1) = run(s)
    f(a1).run(s1)
  }
}

object Stack {

  // Monad instance for my Stack Monad
  implicit val stackMonad: Monad[Stack] = new Monad[Stack] {

    override def pure[A](x: A): Stack[A] =
      Stack(s => (s, x))

    override def flatMap[A, B](fa: Stack[A])(f: A => Stack[B]): Stack[B] =
      fa flatMap f

    override def tailRecM[A, B](a: A)(f: A => Stack[Either[A, B]]): Stack[B] =
      ???
  }

  def empty[A]: List[A] = List.empty[A]

  def init(s: List[Int]): Stack[Unit] =
    Stack(_ => (s, ()))

  def reset: Stack[Unit] =
    Stack(_ => (empty[Int], ()))

  def push(v: Int): Stack[Unit] =
    Stack(s => (v :: s, ()))

  def pop: Stack[Int] = Stack { s =>
    (s.tail, s.head)
  }

  def peek: Stack[Int] = Stack { s =>
    (s, s.head)
  }

  def view: Stack[List[Int]] = Stack { s =>
    (s, s)
  }
}
