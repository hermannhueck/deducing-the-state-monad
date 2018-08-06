/*
  Inspired by "Learn you haskell for grat good"
  http://learnyouahaskell.com/for-a-few-monads-more#state
 */

package stack

import cats.Monad

import scala.util.{Failure, Success, Try}

/*
  Immutable stack implementation, version 4

  If we carefully look at the implementation in version 3 we can see little Stack-specific code.
  What makes it Stack-specific is the IntStack which is just a specific kind of State.

  In version 4 (this impl) I abstracted away the IntStack to a general state S.
  The object Stack keeps it name as it rally contains Stack-specific functions push, pop, etc.
  I renamed case class Stack to State and gave it an additional type Parameter S (which was IntStack before).
  This change propagates through the hole case class State
  and the implicit Monad instance, which now becomes a def due to the new type parameter.

  Client code remains unchanged as I defined a new type alias:

      type Stack[A] = State[IntStack, A]

  which maps the type Stack[A] to State[IntStack, A]
  and the new method:

      def Stack[A](run: IntStack => (IntStack, A)): State[IntStack, A] = State[IntStack, A](run)

  which propagates the invocation of Stack(...) to the invokation of State.apply(...)

  As said, these two additions allow me to treat the client code as before,
  hiding that we indeed work with the more abstract State case class.
 */
object Stack5AbstractingState extends App {

  println("\n-----")

  case class State[S, A](run: S => (S, A)) {

    def runS: S => S = run(_)._1

    def runA: S => A = run(_)._2

    // map doesn't manipulate the state, it just transforms the A value
    def map[B](f: A => B): State[S, B] = State {
      s => {
        val (s1, a1) = run(s)
        (s1, f(a1))
      }
    }

    def flatMap[B](f: A => State[S, B]): State[S, B] = State {
      s => {
        val (s1, a1) = run(s)
        f(a1).run(s1)
      }
    }
  }

  // Monad instance for my State Monad
  implicit def stateMonad[S]: Monad[State[S, ?]] = new Monad[State[S, ?]] {

    override def pure[A](x: A): State[S, A] =
      State { s => (s, x) }

    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
      fa flatMap f

    override def tailRecM[A, B](a: A)(f: A => State[S, Either[A, B]]): State[S, B] =
      ???
  }


  type IntStack = List[Int]
  type Stack[A] = State[IntStack, A]

  def Stack[A](run: IntStack => (IntStack, A)): State[IntStack, A] = State[IntStack, A](run)

  object Stack {

    val EmptyStack = List.empty[Int]

    def init(s: IntStack): Stack[Unit] = Stack {
      _ => (s, ())
    }

    def reset: Stack[Unit] =  Stack {
      _ => (EmptyStack, ())
    }

    def push(v: Int): Stack[Unit] = Stack {
      s => (v :: s, ())
    }

    def pop: Stack[Int] = Stack {
      s => (s.tail, s.head)
    }

    def peek: Stack[Int] = Stack {
      s => (s, s.head)
    }

    def view: Stack[IntStack] = Stack {
      s => (s, s)
    }
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
