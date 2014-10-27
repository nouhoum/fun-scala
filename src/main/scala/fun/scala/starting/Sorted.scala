package fun.scala.starting

object Sorted {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int, acc: Boolean): Boolean = {
      if (n > as.length - 2) acc
      else loop(n + 1, ordered(as(n), as(n + 1)) && acc)
    }
    loop(0, true)
  }
}
