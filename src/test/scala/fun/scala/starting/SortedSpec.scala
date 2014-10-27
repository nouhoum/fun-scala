package fun.scala.starting

import org.specs2.mutable._

class SortedSpec extends Specification {
  "Sorted#isSorted" should {
    "return true" in {
      Sorted.isSorted[Int](Array(1), (a, b) => a <= b) === true
      Sorted.isSorted[Int](Array(), (a, b) => a <= b) === true
      Sorted.isSorted[Int](Array(1, 2, 3), (a, b) => a <= b) === true
      Sorted.isSorted[Int](Array(1, 2, 3, 12, 33), (a, b) => a <= b) === true
      Sorted.isSorted[Int](Array(1, 33, 33), (a, b) => a <= b) === true
    }
    "return false" in {
      Sorted.isSorted[Int](Array(3, 2, 3), (a, b) => a <= b) === false
      Sorted.isSorted[Int](Array(112, 33), (a, b) => a <= b) === false
      Sorted.isSorted[Int](Array(1, 33, 33, 30), (a, b) => a <= b) === false
    }
  }
}
