package pushka

import java.util.UUID

class DefaultRWs extends Generated {

  implicit def optionRW[T](implicit r: Reader[T], w: Writer[T]): RW[Option[T]] = new RW[Option[T]] {
    def read(value: Ast): Option[T] = value match {
      case Ast.Null ⇒ None
      case _ ⇒ Some(r.read(value))
    }

    def write(value: Option[T]): Ast = value match {
      case None ⇒ Ast.Null
      case Some(x) ⇒ w.write(x)
    }
  }

  implicit def eitherRW[Left, Right](implicit leftRw: RW[Left], rightRw: RW[Right]): RW[Either[Left, Right]] = {
    new RW[Either[Left, Right]] {
      def write(value: Either[Left, Right]): Ast = value match {
        case Left(x) ⇒ Ast.Obj(Map("left" → leftRw.write(x)))
        case Right(x) ⇒ Ast.Obj(Map("right" → rightRw.write(x)))
      }

      def read(value: Ast): Either[Left, Right] = value match {
        case Ast.Obj(m) if m.contains("left") ⇒ Left(leftRw.read(m("left")))
        case Ast.Obj(m) if m.contains("right") ⇒ Right(rightRw.read(m("right")))
        case _ ⇒ throw PushkaException()
      }
    }
  }

  implicit val astRW = new RW[Ast] {
    def read(value: Ast): Ast = value
    def write(value: Ast): Ast = value
  }

  implicit val unitRW = new RW[Unit] {
    def read(value: Ast): Unit = ()
    def write(value: Unit): Ast = Ast.Str("_")
  }

  implicit val booleanRW = new RW[Boolean] {
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

  implicit val uuidRW = new RW[UUID] {
    def write(value: UUID): Ast = Ast.Str(value.toString)

    def read(value: Ast): UUID = value match {
      case Ast.Str(s) ⇒ UUID.fromString(s)
      case _ ⇒ throw PushkaException()
    }
  }

  private def writeIterable[T](value: Iterable[T])(implicit w: Writer[T]): Ast = {
    val b = collection.mutable.Buffer.empty[Ast]
    val iter = value.iterator
    while (iter.hasNext) {
      b += w.write(iter.next())
    }
    Ast.Arr(b)
  }

  implicit def seqW[T](implicit w: Writer[T]): Writer[Seq[T]] = new Writer[Seq[T]] {
    def write(value: Seq[T]): Ast = writeIterable(value)
  }

  implicit def setW[T](implicit w: Writer[T]): Writer[Set[T]] = new Writer[Set[T]] {
    def write(value: Set[T]): Ast = writeIterable(value)
  }

  implicit def listW[T](implicit w: Writer[T]): Writer[List[T]] = new Writer[List[T]] {
    def write(value: List[T]): Ast = writeIterable(value)
  }

  implicit def mapW[K, V](implicit w: Writer[(K, V)]): Writer[Map[K, V]] = new Writer[Map[K, V]] {
    def write(value: Map[K, V]): Ast = writeIterable(value)
  }

  implicit def seqR[T](implicit r: Reader[T]): Reader[Seq[T]] = new Reader[Seq[T]] {
    def read(value: Ast): Seq[T] = value match {
      case Ast.Arr(xs) ⇒ xs.map(r.read).toSeq
      case _ ⇒ throw PushkaException()
    }
  }

  implicit def setR[T](implicit r: Reader[T]): Reader[Set[T]] = new Reader[Set[T]] {
    def read(value: Ast): Set[T] = value match {
      case Ast.Arr(xs) ⇒ xs.map(r.read).toSet
      case _ ⇒ throw PushkaException()
    }
  }

  implicit def listR[T](implicit r: Reader[T]): Reader[List[T]] = new Reader[List[T]] {
    def read(value: Ast): List[T] = value match {
      case Ast.Arr(xs) ⇒ xs.map(r.read).toList
      case _ ⇒ throw PushkaException()
    }
  }

  implicit def mapR[K, V](implicit r: Reader[(K, V)]): Reader[Map[K, V]] = {
    new Reader[Map[K, V]] {
      def read(value: Ast): Map[K, V] = value match {
        case Ast.Arr(xs) ⇒ (for (x ← xs) yield r.read(x)).toMap
        case _ ⇒ throw PushkaException()
      }
    }
  }
}
