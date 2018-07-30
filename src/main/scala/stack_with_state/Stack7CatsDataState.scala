/*
  Inspired by "Learn you haskell for grat good"
  http://learnyouahaskell.com/for-a-few-monads-more#state
 */

package stack_with_state

import java.util.NoSuchElementException

import cats.data.State

import scala.util.{Failure, Success, Try}

/*
  Immutable stack implementation, version 6

  I could delete my implementation of State (case class an companion object)
  and instead import cats.data.State. I also deleted the Monad instance for State
  as this is also provided by Cats.

  The Cats State Monad internally uses the Eval Monad
  Hence I had to add an invocation of '.value' at several places
  in order to get access to the wrapped function s => (s, a)

  Having done this the same code works like charm with the Cats State Monad interface
  which shows that my implementation in version 5 wasn't too bad.
 */
object Stack7CatsDataState extends App {

  println("\n-----")

  type IntStack = List[Int]
  type Stack[A] = State[IntStack, A]

  def Stack[A](run: IntStack => (IntStack, A)): State[IntStack, A] = State[IntStack, A](run)

  object Stack {

    val EmptyStack = List.empty[Int]

    def init(s: IntStack): Stack[Unit] = State.set(s)

    def reset: Stack[Unit] = init(EmptyStack)

    def push(v: Int): Stack[Unit] = State.modify(v :: _)

    def pop: Stack[Int] = State { s =>
      val a1 = State.get[IntStack].map(_.head).runA(s)
      val s2 = State.modify[IntStack](_.tail).runS(s)
      (s2.value, a1.value)
    }

    def peek: Stack[Int] = State.get[IntStack].map(_.head)

    def view: Stack[IntStack] = State.get[IntStack]
  }


  import Stack._

  val stack1: Stack[Int] = for {
    _ <- init(List(5,8,2,1))
    _ <- push(3)
    _ <- push(5)
    _ <- push(7)
    _ <- pop
    _ <- pop
    s_a <- peek
  } yield s_a

  checkResult(stack1.run(EmptyStack).value, List(3, 5, 8, 2, 1), 3)

  val stack2: Stack[Int] = stack1.map(_ + 100)
  checkResult(stack2.run(EmptyStack).value, List(3, 5, 8, 2, 1), 103)

  val stack3: Stack[Int] = stack2.flatMap(_ => push(42)).flatMap(_ => pop)
  checkResult(stack3.run(EmptyStack).value, List(3, 5, 8, 2, 1), 42)

  val stack4: Stack[IntStack] = stack3.flatMap(_ => view)
  val (s4, v4) = stack4.run(EmptyStack).value
  println((s4, v4))
  assert(s4 == List(3, 5, 8, 2, 1) && v4 == s4)

  val stack5: Stack[Int] = for {
    _ <- reset
    - <- push(3)
    _ <- pop
    s <- pop
  } yield s

  Try {
    stack5.run(EmptyStack).value
  } match {
    case Success(value) =>
      assert(assertion = false, "Expected exception didn't occur!")
    case Failure(t) =>
      t match {
        case e: NoSuchElementException =>
          println(s"Got NoSuchElementException as expected: ${e.getMessage}")
          assert(true)
        case e: UnsupportedOperationException =>
          println(s"Got UnsupportedOperationException as expected: ${e.getMessage}")
          assert(true)
        case e =>
          assert(assertion = false, s"Unexpected Exception: $e")
      }
  }



  private def checkResult(tuple2: (IntStack, Int), expectedStack: IntStack, expectedValue: Int): Unit = {

    val (intStack, value) = tuple2

    println(intStack)
    println(value)

    assert(intStack == expectedStack && value == expectedValue)
  }


  println("-----")
}
