/*
  Inspired by "Learn you haskell for grat good"
  http://learnyouahaskell.com/for-a-few-monads-more#state
 */

package stack_with_state

import java.util.NoSuchElementException

import cats.Monad

import scala.util.{Failure, Success, Try}

/*
  Immutable stack implementation, version 5

  In this version I added the companion object State which implements five convenience methods
  for State processing: pure, modify, inspect, get and set,
  just the same methods that Cats provides in the State companion object.

  Then I reimplemented the stack methods push, pop etc. with the convenience methods of State

  With that I provide a semantically identical State Monad interface as Cats.
 */
object Stack6StateObjectConvenienceFunctions extends App {

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

  object State {

    /**
      * Return `a` and maintain the input state.
      */
    def pure[S, A](a: A): State[S, A] = State {
      s => (s, a)
    }

    /**
      * Modify the input state and return Unit.
      */
    def modify[S](f: S => S): State[S, Unit] = State {
      s => (f(s), ())
    }

    /**
      * Inspect a value from the input state, without modifying the state.
      */
    def inspect[S, T](f: S => T): State[S, T] = State {
      s => (s, f(s))
    }

    /**
      * Return the input state without modifying it.
      */
    def get[S]: State[S, S] = State {
      s => (s, s)
    }
    // same as: inspect(identity)

    /**
      * Set the state to `s` and return Unit.
      */
    def set[S](s: S): State[S, Unit] = State {
      _ => (s, ())
    }
    // same as: modify(_ => s)
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

    def init(s: IntStack): Stack[Unit] = State.set(s)

    def reset: Stack[Unit] = init(EmptyStack)

    def push(v: Int): Stack[Unit] = State.modify(v :: _)

    def pop: Stack[Int] = State { s =>
      val a1 = State.get[IntStack].map(_.head).runA(s)
      val s2 = State.modify[IntStack](_.tail).runS(s)
      (s2, a1)
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
