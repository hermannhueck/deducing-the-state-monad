/*
  Inspired by "Learn you haskell for grat good"
  http://learnyouahaskell.com/for-a-few-monads-more#state
 */

package stack

import cats.Monad

import scala.util.{Failure, Success, Try}

import util._

/*
  Immutable stack implementation, version 3

  I defined a case class Stack which encapsulates a stack function named run:

      case class Stack[A](run: IntStack => (IntStack, A))

  As before the wrapped 'run' function takes a stack and returns a Tuple2 with the new stack and the vlaue of type A.
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
object Stack4Monad extends App {

  printStartLine()

  type IntStack = List[Int]

  case class Stack[A](run: IntStack => (IntStack, A)) {

    def runS: IntStack => IntStack = run(_)._1

    def runA: IntStack => A = run(_)._2

    // map doesn't manipulate the stack, it just transforms the A value
    def map[B](f: A => B): Stack[B] = Stack { s =>
      {
        val (s1, a1) = run(s)
        (s1, f(a1))
      }
    }

    def flatMap[B](f: A => Stack[B]): Stack[B] = Stack { s =>
      {
        val (s1, a1) = run(s)
        f(a1).run(s1)
      }
    }
  }

  // Monad instance for my Stack Monad
  implicit val stackMonad: Monad[Stack] = new Monad[Stack] {

    override def pure[A](x: A): Stack[A] =
      Stack { s =>
        (s, x)
      }

    override def flatMap[A, B](fa: Stack[A])(f: A => Stack[B]): Stack[B] =
      fa flatMap f

    override def tailRecM[A, B](a: A)(f: A => Stack[Either[A, B]]): Stack[B] =
      ???
  }

  object Stack {

    val EmptyStack = List.empty[Int]

    def init(s: IntStack): Stack[Unit] = Stack { _ =>
      (s, ())
    }

    def reset: Stack[Unit] = Stack { _ =>
      (EmptyStack, ())
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

  import Stack._

  val stack1: Stack[Int] = for {
    _ <- init(List(5, 8, 2, 1))
    _ <- push(3)
    _ <- push(5)
    _ <- push(7)
    _ <- pop
    _ <- pop
    s_a <- peek
  } yield s_a

  checkResult(stack1.run(EmptyStack), List(3, 5, 8, 2, 1), 3)

  val stack2: Stack[Int] = stack1.map(_ + 100)
  checkResult(stack2.run(EmptyStack), List(3, 5, 8, 2, 1), 103)

  val stack3: Stack[Int] = stack2.flatMap(_ => push(42)).flatMap(_ => pop)
  checkResult(stack3.run(EmptyStack), List(3, 5, 8, 2, 1), 42)

  val stack4: Stack[IntStack] = stack3.flatMap[IntStack](_ => view)
  val (s4, v4) = stack4.run(EmptyStack)
  println((s4, v4))
  assert(s4 == List(3, 5, 8, 2, 1) && v4 == s4)

  val stack5: Stack[Int] = for {
    _ <- reset
    - <- push(3)
    _ <- pop
    s <- pop
  } yield s

  Try {
    stack5.run(EmptyStack)
  } match {
    case Success(value) =>
      assert(assertion = false, "Expected exception didn't occur!")
    case Failure(t) =>
      t match {
        case e: NoSuchElementException =>
          println(s"Got NoSuchElementException as expected: ${e.getMessage}")
          assert(true)
        case e: UnsupportedOperationException =>
          println(
            s"Got UnsupportedOperationException as expected: ${e.getMessage}"
          )
          assert(true)
        case e =>
          assert(assertion = false, s"Unexpected Exception: $e")
      }
  }

  private def checkResult(
      tuple2: (IntStack, Int),
      expectedStack: IntStack,
      expectedValue: Int
  ): Unit = {

    val (intStack, value) = tuple2

    println(intStack)
    println(value)

    assert(intStack == expectedStack && value == expectedValue)
  }

  printEndLine()
}
