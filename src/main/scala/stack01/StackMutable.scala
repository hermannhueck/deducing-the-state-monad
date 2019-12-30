/*
  Inspired by "Learn you haskell for grat good"
  http://learnyouahaskell.com/for-a-few-monads-more#state
 */

package stack01

import util._

/*
  Mutable stack implementation

  Stack.stack is a var that is manipulated by the methods of the object Stack: push, pop etc.
 */
object StackMutable extends App {

  import Stack._

  printStartLine()

  val v0: Unit = init(Stack(5, 8, 2, 1))
  println((v0, stack))
  assert(stack == Stack(5, 8, 2, 1))

  val v1: Unit = push(3)
  println((v1, stack))
  assert(stack == Stack(3, 5, 8, 2, 1))

  val v2: Unit = push(5)
  println((v2, stack))
  assert(stack == Stack(5, 3, 5, 8, 2, 1))

  val v3: Unit = push(7)
  println((v3, stack))
  assert(stack == Stack(7, 5, 3, 5, 8, 2, 1))

  val v4: Int = pop
  println((v4, stack))
  assert(stack == Stack(5, 3, 5, 8, 2, 1) && v4 == 7)

  val v5: Int = pop
  println((v5, stack))
  assert(stack == Stack(3, 5, 8, 2, 1) && v5 == 5)

  val v6: Int = peek
  println((v6, stack))
  assert(stack == Stack(3, 5, 8, 2, 1) && v6 == 3)

  val v7: Stack[Int] = view
  println((v7, stack))
  assert(stack == Stack(3, 5, 8, 2, 1) && v7 == stack)

  printEndLine()
}
