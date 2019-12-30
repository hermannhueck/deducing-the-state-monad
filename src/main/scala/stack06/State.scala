package stack06

import cats.Monad

case class State[S, A](run: S => (S, A)) {

  def runS: S => S =
    s => run(s)._1

  def runA: S => A =
    s => run(s)._2

  // map doesn't manipulate the state, it just transforms the A value
  def map[B](f: A => B): State[S, B] =
    State { s =>
      val (s1, a1) = run(s)
      (s1, f(a1))
    }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (s1, a1) = run(s)
      f(a1).run(s1)
    }
}

object State {

  // Monad instance for my State Monad
  implicit def stateMonad[S]: Monad[State[S, ?]] = new Monad[State[S, ?]] {

    override def pure[A](x: A): State[S, A] =
      State(s => (s, x))

    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
      fa flatMap f

    override def tailRecM[A, B](a: A)(f: A => State[S, Either[A, B]]): State[S, B] =
      ???
  }

  /**
    * Return `a` and maintain the input state.
    */
  def pure[S, A](a: A): State[S, A] =
    State { s =>
      (s, a)
    }

  /**
    * Modify the input state and return Unit.
    */
  def modify[S](f: S => S): State[S, Unit] =
    State { s =>
      (f(s), ())
    }

  /**
    * Inspect a value from the input state, without modifying the state.
    */
  def inspect[S, T](f: S => T): State[S, T] =
    State { s =>
      (s, f(s))
    }

  /**
    * Return the input state without modifying it.
    */
  def get[S]: State[S, S] =
    State { s =>
      (s, s)
    }
  // same as: inspect(identity)

  /**
    * Set the state to `s` and return Unit.
    */
  def set[S](s: S): State[S, Unit] =
    State { _ =>
      (s, ())
    }
  // same as: modify(_ => s)
}
