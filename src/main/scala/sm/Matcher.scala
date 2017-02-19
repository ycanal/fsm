package sm

trait Matcher[T, U] extends ((T, U) => Boolean) {
  def |(that: Matcher[T, U]): Matcher[T, U] = {
    Matcher((b: T, u: U) => this(b, u) || that(b, u))
  }
  def &(that: Matcher[T, U]): Matcher[T, U] = {
    Matcher((b: T, u: U) => this(b, u) && that(b, u))
  }
}

object Matcher {
  def apply[T, U](f: (T, U) => Boolean): Matcher[T, U] = new Matcher[T, U] {
    def apply(b: T, u: U): Boolean = f(b, u)
  }
}
