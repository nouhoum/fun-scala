package fun.scala

object Fib {
  def fib(n: Int): Int = {
    if (n == 0 || n == 1) n
    else fib(n - 1) + fib(n - 2)
  }

  def fib2(n: Int): Int = {
    @annotation.tailrec
    def loop(m: Int, previous: Int, current: Int): Int = {
      if (m == 0) previous
      else loop(m - 1, current, previous + current)
    }

    loop(n, 0, 1)
  }
}
