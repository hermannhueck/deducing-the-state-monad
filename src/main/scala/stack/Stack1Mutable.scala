/*
  Inspired by "Learn you haskell for grat good"
  http://learnyouahaskell.com/for-a-few-monads-more#state
 */

package stack

/*
  Mutable stack implementation

  Stack.stack is a var that is manipulated by the methods of the object Stack: push, pop etc.
 */
object Stack1Mutable extends App {

  println("\n-----")

  type IntStack = List[Int]

  object Stack {

    val EmptyStack = List.empty[Int]

    var stack: IntStack = EmptyStack

    def init(s: IntStack): Unit = stack = s

    def reset(): Unit = init(EmptyStack)

    def push(v: Int): Unit = stack = v :: stack

    def pop: Int = {
      val h = stack.head
      stack = stack.tail
      h
    }

    def peek: Int = stack.head

    def view: IntStack = stack
  }

  import Stack._

  val v0: Unit = init(List(5, 8, 2, 1))
  println((v0, stack))
  assert(stack == List(5, 8, 2, 1))

  val v1: Unit = push(3)
  println((v1, stack))
  assert(stack == List(3, 5, 8, 2, 1))

  val v2: Unit = push(5)
  println((v2, stack))
  assert(stack == List(5, 3, 5, 8, 2, 1))

  val v3: Unit = push(7)
  println((v3, stack))
  assert(stack == List(7, 5, 3, 5, 8, 2, 1))

  val v4 = pop
  println((v4, stack))
  assert(stack == List(5, 3, 5, 8, 2, 1) && v4 == 7)

  val v5 = pop
  println((v5, stack))
  assert(stack == List(3, 5, 8, 2, 1) && v5 == 5)

  val v6 = peek
  println((v6, stack))
  assert(stack == List(3, 5, 8, 2, 1) && v6 == 3)

  val v7 = view
  println((v7, stack))
  assert(stack == List(3, 5, 8, 2, 1) && v7 == stack)

  println("-----")
}
