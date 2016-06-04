package pushka.internal

import scala.language.higherKinds

final class Not[A](val x: Int) extends AnyVal

object Not {
  implicit def notA[A, TC[_]] = new Not[TC[A]](0)
  implicit def notNotA[A : TC, TC[_]] = new Not[TC[A]](0)
}
