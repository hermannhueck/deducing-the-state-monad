/*
  Inspired by "Learn you haskell for grat good"
  http://learnyouahaskell.com/for-a-few-monads-more#state
 */

package stack07

/*
  Immutable stack implementation, using the cat.data.State Monad

  I could delete my implementation of State (case class an companion object)
  and instead import cats.data.State. I also deleted the Monad instance for State
  as this is also provided by Cats.

  The Cats State Monad internally uses the Eval Monad
  Hence I had to add an invocation of '.value' at several places
  in order to get access to the wrapped function s => (s, a)

  Having done this the same code works like a charm with the Cats State Monad interface
  which shows that my implementation in the previous version wasn't too bad.
 */
object StackWithCatsStateMonad extends App {

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

  checkResult(stack1.run(empty[Int]).value, List(3, 5, 8, 2, 1), 3)

  val stack2: Stack[Int] = stack1.map(_ + 100)
  checkResult(stack2.run(empty[Int]).value, List(3, 5, 8, 2, 1), 103)

  val stack3: Stack[Int] = stack2.flatMap(_ => push(42)).flatMap(_ => pop)
  checkResult(stack3.run(empty[Int]).value, List(3, 5, 8, 2, 1), 42)

  val stack4: Stack[IntStack] = stack3.flatMap(_ => view)
  val (s4, v4)                = stack4.run(empty[Int]).value
  println((s4, v4))
  assert(s4 == List(3, 5, 8, 2, 1) && v4 == s4)

  val stack5: Stack[Int] = for {
    _ <- reset
    - <- push(3)
    _ <- pop
    s <- pop
  } yield s

  checkFailureOccured(stack5.run(empty[Int]).value)

  printEndLine()
}
