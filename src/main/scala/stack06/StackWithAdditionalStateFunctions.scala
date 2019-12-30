/*
  Inspired by "Learn you haskell for grat good"
  http://learnyouahaskell.com/for-a-few-monads-more#state
 */

package stack06

/*
  Immutable stack implementation, using additional State functions

  In this version I added the companion object State which implements five convenience methods
  for State processing: pure, modify, inspect, get and set,
  just the same methods that Cats provides in the State companion object.

  Then I reimplemented the stack methods push, pop etc. with these convenience methods of State

  With that I provide a semantically identical State Monad interface as Cats.
 */
object StackWithAdditionalStateFunctions extends App {

  import util._
  import helper._
  import Stack._

  printStartLine()

  val stack1: Stack[Int] = for {
    _   <- init(List(5, 8, 2, 1))
    _   <- push(3)
    _   <- push(5)
    _   <- push(7)
    _   <- pop
    _   <- pop
    s_a <- peek
  } yield s_a

  checkResult(stack1.run(empty[Int]), List(3, 5, 8, 2, 1), 3)

  val stack2: Stack[Int] = stack1.map(_ + 100)
  checkResult(stack2.run(empty[Int]), List(3, 5, 8, 2, 1), 103)

  val stack3: Stack[Int] = stack2.flatMap(_ => push(42)).flatMap(_ => pop)
  checkResult(stack3.run(empty[Int]), List(3, 5, 8, 2, 1), 42)

  val stack4: Stack[IntStack] = stack3.flatMap[IntStack](_ => view)
  val (s4, v4)                = stack4.run(empty[Int])
  println((s4, v4))
  assert(s4 == List(3, 5, 8, 2, 1) && v4 == s4)

  val stack5: Stack[Int] = for {
    _ <- reset
    - <- push(3)
    _ <- pop
    s <- pop
  } yield s

  checkFailureOccured(stack5.run(empty[Int]))

  printEndLine()
}
