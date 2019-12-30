/*
  Inspired by "Learn you haskell for great good"
  http://learnyouahaskell.com/for-a-few-monads-more#state
 */

package stack05

/*
  Immutable stack implementation, with State abstracted (previously fixed to List[Int])

  If we carefully look at the implementation in the previous version, we can see little Stack-specific code.
  What makes it Stack-specific is the IntStack (= Lisyt[Int]) which is just a specific kind of State.

  In this impl I abstracted away the IntStack to a general state S.
  The object Stack keeps it name as it really contains Stack-specific functions push, pop, etc.
  I renamed case class Stack to State and gave it an additional type Parameter S (which was IntStack before).
  This change propagates through the hole case class State
  and the implicit Monad instance, which now becomes a def due to the new type parameter.

  Client code remains unchanged as I defined a new type alias:

      type IntStack = List[Int]
      type Stack[A] = State[IntStack, A]

  which maps the type Stack[A] to State[IntStack, A]
  and the new method Stack.apply:

      def apply[A](run: IntStack => (IntStack, A)): State[IntStack, A] =
        State[IntStack, A](run)

  which propagates the invocation of Stack.apply(run) to the invokation of State.apply(run)

  As said, these two additions allow me to treat the client code as before,
  hiding the fact that we indeed work with the more abstract State case class.
 */
object StackAbstractingState extends App {

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
