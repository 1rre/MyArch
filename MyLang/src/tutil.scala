package object tutil {
  def trace[T](x: T): T = {
    println(x)
    x
  }
}