/*
  Inspired by "Learn you haskell for grat good"
  http://learnyouahaskell.com/for-a-few-monads-more#state
 */

package stack04

/*
  Immutable stack implementation, using cats.Monad

  I defined a case class Stack which encapsulates a stack function named run:

      case class Stack[A](run: List[Int] => (List[Int], A))

  As before the wrapped 'run' function takes a stack and returns a Tuple2 with the new stack and the value of type A.
  Therefore the Stack case class is parameterized with type A.
  The function 'runS' returns only the stack part of the Tuple2.
  The function 'runA' returns only the value part of the Tuple2.

  To make the stack monadic I additionally implemented map and flatMap.
  'map' leaves the stack unchanged. It only transforms the value from A => B.
  'flatMap' possibly manipulates the stack and the value, depending on the function passed to it.

  Having map and flatMap in the Stack case class makes it easy to provide a Monad instance as an implicit val.

  Now we can use the stack functions (push, pop etc) in monadic context, i.e.
  we can pass the to flatMap or use them in a for-comprehension as demonstrated below.
 */
object StackWithCatsMonad extends App {

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

  val stack4: Stack[List[Int]] = stack3.flatMap[List[Int]](_ => view)
  val (s4, v4)                 = stack4.run(empty[Int])
  println((s4, v4))
  assert(s4 == List(3, 5, 8, 2, 1) && v4 == s4)

  val stack5: Stack[Int] = for {
    _ <- reset
    _ <- push(3)
    _ <- pop
    s <- pop
  } yield s

  checkFailureOccured(stack5.run(empty[Int]))

  printEndLine()
}
