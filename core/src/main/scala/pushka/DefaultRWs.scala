package pushka

class DefaultRWs {

  implicit def option[T](implicit r: Reader[T], w: Writer[T]): RW[Option[T]] = new RW[Option[T]] {
    def read(value: Ast): Option[T] = value match {
      case Ast.Null ⇒ None
      case _ ⇒ Some(r.read(value))
    }
    def write(value: Option[T]): Ast = value match {
      case None ⇒ Ast.Null
      case Some(x) ⇒ w.write(x)
    }
  }

  implicit val boolean = new RW[Boolean] {
    def read(value: Ast): Boolean = value match {
      case Ast.True ⇒ true
      case Ast.False ⇒ false
      case _ ⇒ throw PushkaException()
    }
    def write(value: Boolean): Ast = {
      if (value) Ast.True else Ast.False
    }
  }

  implicit val int = new RW[Int] {
    def read(value: Ast): Int = value match {
      case Ast.Num(x) ⇒ x.toInt
      case _ ⇒ throw PushkaException()
    }
    def write(value: Int): Ast = {
      Ast.Num(value.toDouble)
    }
  }

  implicit val double = new RW[Double] {
    def read(value: Ast): Double = value match {
      case Ast.Num(x) ⇒ x
      case _ ⇒ throw PushkaException()
    }
    def write(value: Double): Ast = {
      Ast.Num(value)
    }
  }

  implicit val float = new RW[Float] {
    def read(value: Ast): Float = value match {
      case Ast.Num(x) ⇒ x.toFloat
      case _ ⇒ throw PushkaException()
    }
    def write(value: Float): Ast = {
      Ast.Num(value.toDouble)
    }
  }

  implicit val long = new RW[Long] {
    def read(value: Ast): Long = value match {
      case Ast.Str(x) ⇒ x.toLong
      case _ ⇒ throw PushkaException()
    }
    def write(value: Long): Ast = {
      Ast.Str(value.toString)
    }
  }

  implicit val string = new RW[String] {
    def read(value: Ast): String = value match {
      case Ast.Str(x) ⇒ x
      case _ ⇒ throw PushkaException()
    }
    def write(value: String): Ast = {
      Ast.Str(value)
    }
  }

  implicit def iterableW[T](implicit w: Writer[T]): Writer[Iterable[T]] = new Writer[Iterable[T]] {
    def write(value: Iterable[T]): Ast = {
      Ast.Arr(value.map(w.write).toList)
    }
  }

  implicit def seqR[T](implicit r: Reader[T]): Reader[Seq[T]] = new Reader[Seq[T]] {
    def read(value: Ast): Seq[T] = value match {
      case Ast.Arr(xs) ⇒ xs.map(r.read)
      case _ ⇒ throw PushkaException()
    }
  }

  implicit def listR[T](implicit r: Reader[T]): Reader[List[T]] = new Reader[List[T]] {
    def read(value: Ast): List[T] = value match {
      case Ast.Arr(xs) ⇒ xs.map(r.read)
      case _ ⇒ throw PushkaException()
    }
  }

  implicit def tuple2[A1, A2](implicit rw1: RW[A1], rw2: RW[A2]): RW[(A1, A2)] = new RW[(A1, A2)] {
    def read(value: Ast): (A1, A2) = value match {
      case Ast.Arr(xs) if xs.length >= 2 ⇒
        (rw1.read(xs.head), rw2.read(xs(1)))
      case _ ⇒ throw PushkaException()
    }
    def write(value: (A1, A2)): Ast = {
      Ast.Arr(List(rw1.write(value._1), rw2.write(value._2)))
    }
  }

  implicit def tuple3[A1, A2, A3](implicit rw1: RW[A1], rw2: RW[A2], rw3: RW[A3]): RW[(A1, A2, A3)] = new RW[(A1, A2, A3)] {
    def read(value: Ast): (A1, A2, A3) = value match {
      case Ast.Arr(xs) if xs.length >= 3 ⇒
        (rw1.read(xs.head), rw2.read(xs(1)), rw3.read(xs(2)))
      case _ ⇒ throw PushkaException()
    }
    def write(value: (A1, A2, A3)): Ast = {
      Ast.Arr(List(
        rw1.write(value._1),
        rw2.write(value._2),
        rw3.write(value._3)
      ))
    }
  }

  // TODO generate tuples?
}
