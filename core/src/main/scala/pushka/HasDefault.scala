package pushka

import pushka.internal.Not

trait HasDefault[T] extends Any {
  def default: Option[HasDefault.Default[T]]
}

object HasDefault {

  trait Default[T] {
    def value: T
    def writeDefault: Boolean
  }

  object Default {
    implicit def optionDefault[T] = new Default[Option[T]] {
      def value = None
      def writeDefault = false
    }
  }

  implicit def hasNotDefault[T](implicit ev: Not[Default[T]]): HasDefault[T] =
    new HasDefault[T] { def default = None }

  implicit def hasDefault[T](implicit ev: Default[T]): HasDefault[T] =
    new HasDefault[T] { def default = Some(ev) }

  def apply[T](implicit ev: HasDefault[T]): Option[Default[T]] = ev.default
}
