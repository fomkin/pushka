package pushka

import scala.util.{Failure, Success, Try}

trait TestMethods {
  implicit class tryAssertionMethods[T](th: Try[T]) {
    def exceptionAssert(str: String): Unit = {
      th match {
        case Success(_) => throw new Exception ("PushkaException should be thrown.")
        case Failure(e: PushkaException) => assert(e.message == str)
        case Failure(e) => throw e
      }
    }
  }
}
