package pushka

class DefaultRWs {

  implicit def option[T](implicit rw1: RW[T]): RW[Option[T]] = new RW[Option[T]] {
    def read(value: Value): Option[T] = value match {
      case Value.Null ⇒ None
      case _ ⇒ Some(rw1.read(value))
    }
    def write(value: Option[T]): Value = value match {
      case None ⇒ Value.Null
      case Some(x) ⇒ rw1.write(x)
    }
  }

  implicit val int = new RW[Int] {
    def read(value: Value): Int = value match {
      case Value.Number(x) ⇒ x.toInt
      case _ ⇒ throw PushkaException()
    }

    def write(value: Int): Value = {
      Value.Number(value.toDouble)
    }
  }

  implicit val string = new RW[String] {
    def read(value: Value): String = value match {
      case Value.Str(x) ⇒ x
      case _ ⇒ throw PushkaException()
    }
    def write(value: String): Value = {
      Value.Str(value)
    }
  }

  implicit def tuple2[A1, A2](implicit rw1: RW[A1], rw2: RW[A2]): RW[(A1, A2)] = new RW[(A1, A2)] {
    def read(value: Value): (A1, A2) = value match {
      case Value.Arr(xs) if xs.length >= 2 ⇒
        (rw1.read(xs.head), rw2.read(xs(1)))
      case _ ⇒ throw PushkaException()
    }
    def write(value: (A1, A2)): Value = {
      Value.Arr(Seq(rw1.write(value._1), rw2.write(value._2)))
    }
  }

  implicit def tuple3[A1, A2, A3](implicit rw1: RW[A1], rw2: RW[A2], rw3: RW[A3]): RW[(A1, A2, A3)] = new RW[(A1, A2, A3)] {
    def read(value: Value): (A1, A2, A3) = value match {
      case Value.Arr(xs) if xs.length >= 3 ⇒
        (rw1.read(xs.head), rw2.read(xs(1)), rw3.read(xs(2)))
      case _ ⇒ throw PushkaException()
    }
    def write(value: (A1, A2, A3)): Value = {
      Value.Arr(Seq(
        rw1.write(value._1),
        rw2.write(value._2),
        rw3.write(value._3)
      ))
    }
  }

}
