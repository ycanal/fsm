package sm

trait Matcher[T] extends (T => Boolean) {
  def |(that: Matcher[T]): Matcher[T] = {
    Matcher((b: T) => this(b) || that(b))
  }
  def &(that: Matcher[T]): Matcher[T] = {
    Matcher((b: T) => this(b) && that(b))
  }
}

object Matcher {
  def apply[T](f: T => Boolean): Matcher[T] = new Matcher[T] {
    def apply(b: T): Boolean = f(b)
  }
}
