/*
  Inspired by "Learn you haskell for grat good"
  http://learnyouahaskell.com/for-a-few-monads-more#state
 */

package stack02

import util._

/*
  Immutable stack implementation

  The var Stack.stack disappeared.
  Every stack method gets the stack passed as a parameter
  and returns the new (possibly manipulated) stack tupled up with the value that was returned before
  (e.g. the Int value popped off or () in case of push).

  Passing the stack into the method and returning the possibly manipulated stack allows for immutability.
  This pattern implements the basic idea of immutable state.
 */
object StackImmutable extends App {

  printStartLine()

  import Stack._

  val (s0, v0) = init(Stack(5, 8, 2, 1))
  println((v0, s0))
  assert(s0 == Stack(5, 8, 2, 1))

  val (s1, v1) = push(s0, 3)
  println((v1, s1))
  assert(s1 == Stack(3, 5, 8, 2, 1))

  val (s2, v2) = push(s1, 5)
  println((v2, s2))
  assert(s2 == Stack(5, 3, 5, 8, 2, 1))

  val (s3, v3) = push(s2, 7)
  println((v3, s3))
  assert(s3 == Stack(7, 5, 3, 5, 8, 2, 1))

  val (s4, v4) = pop(s3)
  println((v4, s4))
  assert(s4 == Stack(5, 3, 5, 8, 2, 1) && v4 == 7)

  val (s5, v5) = pop(s4)
  println((v5, s5))
  assert(s5 == Stack(3, 5, 8, 2, 1) && v5 == 5)

  val (s6, v6) = peek(s5)
  println((v6, s6))
  assert(s6 == Stack(3, 5, 8, 2, 1) && v6 == 3)

  val (s7, v7) = view(s6)
  println((v7, s7))
  assert(s7 == Stack(3, 5, 8, 2, 1) && v7 == s7)

  printEndLine()
}
