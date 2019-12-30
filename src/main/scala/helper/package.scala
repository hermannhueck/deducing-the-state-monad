package object helper {

  import java.util.NoSuchElementException

  import scala.util.{Failure, Success, Try}

  def checkFailureOccured[STACK, A](stackRun: => (STACK, A)): Unit =
    Try(stackRun) match {
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

  def checkResult[STACK, A](tuple2: (STACK, A), expectedStack: STACK, expectedValue: A): Unit = {

    val (stack, value) = tuple2

    println(stack)
    println(value)

    assert(stack == expectedStack && value == expectedValue)
  }

}
