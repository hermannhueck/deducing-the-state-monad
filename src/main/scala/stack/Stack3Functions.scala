/*
  Inspired by "Learn you haskell for grat good"
  http://learnyouahaskell.com/for-a-few-monads-more#state
 */

package stack

import util._

/*
  Immutable stack implementation, version 2

  The stack methods have been changed to functions.
  All functions abide by the same pattern:
    s => (sNew, value)
  They take a stack and return a Tuple2 containing the new Stack alongside the value.
  The type of the value might be different for every stack function.

  The type alias
      type Stack[A] = IntStack => (IntStack, A)
  is 'hiding' the fact that a function is returned.
 */
object Stack3Functions extends App {

  printStartLine()

  type IntStack = List[Int]
  type Stack[A] = IntStack => (IntStack, A)

  object Stack {

    val EmptyStack = List.empty[Int]

    val init: Stack[Unit] =
      s => (s, ())

    val reset: Stack[Unit] =
      _ => init(EmptyStack)

    def push(v: Int): Stack[Unit] =
      s => (v :: s, ())

    val pop: Stack[Int] =
      s => (s.tail, s.head)

    val peek: Stack[Int] =
      s => (s, s.head)

    val view: Stack[IntStack] =
      s => (s, s)
  }

  import Stack._

  val (s0, v0) = init(List(5, 8, 2, 1))
  println((v0, s0))
  assert(s0 == List(5, 8, 2, 1))

  val (s1, v1) = push(3)(s0)
  println((v1, s1))
  assert(s1 == List(3, 5, 8, 2, 1))

  val (s2, v2) = push(5)(s1)
  println((v2, s2))
  assert(s2 == List(5, 3, 5, 8, 2, 1))

  val (s3, v3) = push(7)(s2)
  println((v3, s3))
  assert(s3 == List(7, 5, 3, 5, 8, 2, 1))

  val (s4, v4) = pop(s3)
  println((v4, s4))
  assert(s4 == List(5, 3, 5, 8, 2, 1) && v4 == 7)

  val (s5, v5) = pop(s4)
  println((v5, s5))
  assert(s5 == List(3, 5, 8, 2, 1) && v5 == 5)

  val (s6, v6) = peek(s5)
  println((v6, s6))
  assert(s6 == List(3, 5, 8, 2, 1) && v6 == 3)

  val (s7, v7) = view(s6)
  println((v7, s7))
  assert(s7 == List(3, 5, 8, 2, 1) && v7 == s7)

  printEndLine()
}
