package fun.scala.lazies

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def toList2: List[A] = {
    @annotation.tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => loop(t(), h() :: acc)
      case _ => acc
    }

    loop(this, Nil) reverse
  }

  import Stream.cons

  def take(n: Int): Stream[A] = {
    if (n > 0) this match {
      case Cons(h, t) if n == 1 => cons(h(), Stream.empty)
      case Cons(h, t) => cons(h(), t().take(n - 1))
      case _ => Stream.empty
    }
    else Stream.empty
  }

  def drop(n: Int): Stream[A] = {
    def loop(s: Stream[A], m: Int): Stream[A] = {
      if (m < 1) s
      else s match {
        case Cons(_, t) => loop(t(), m - 1)
        case _ => Stream()
      }
    }
    loop(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if (p(h())) => cons(h(), t().takeWhile(p))
      case _ => Stream()
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def forAll2(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) =>
      if (p(a)) Cons(() => a, () => b)
      else Stream.empty
    )

  def headOption2: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((a, b) =>
    cons(f(a), b)
  )

  def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, t) =>
    if (p(a)) cons(a, t) else t
  )

  def append[B >: A](o: => Stream[B]): Stream[B] = foldRight(o)((a, acc) =>
    cons(a, acc)
  )

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, t) =>
    f(a) append t
  )

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = (this, s2) match {
    case (Cons(h, t), Cons(h2, t2)) => Cons(() => (Some(h()), Some(h2())), () => t().zipAll(t2()))
    case (Cons(h, t), Empty) => Cons(() => (Some(h()), None), () => t().zipAll(Empty))
    case (Empty, Cons(h, t)) => Cons(() => (None, Some(h())), () => Empty.zipAll(t()))
    case (Empty, Empty) => Empty
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, _) | (_, Empty) => None
      case (Cons(h, t), Cons(h2, t2)) => Some((f(h(), h2()), (t(), t2())))
    }

  def zipAll2[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWith(s2)((a, b) => (Some(a), Some(b)))

  def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }

  def take2(n: Int): Stream[A] = {
    if (n > 0)
      Stream.unfold(this) {
        case Empty => None
        case Cons(h, t) => Some((h(), t().take(n - 1)))
      }
    else
      Stream.empty[A]
  }

  def takeViaUnFold(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (Cons(h, t), n) if n == 0 => None
      case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  def takeWhile2(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined).forAll {
      case (e1, e2) => e1 == e2
    }

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case s @ Cons(h, t) => Some((s, t()))
      case _ => None
    }

  def tails22: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } append (Stream(Stream.empty))

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    tails.map(_.foldRight(z)(f))
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = {
    lazy val tail = Cons(() => n, () => from(n + 1))
    tail
  }

  def fibs(): Stream[Int] = {
    def loop(n0: Int, n1: Int): Stream[Int] = {
      lazy val tail: Stream[Int] = Cons(() => n0, () => loop(n1, n0 + n1))
      tail
    }

    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).fold(Stream.empty[A])(v => {
      lazy val a = v._1
      lazy val s = v._2
      Cons(() => a, () => unfold(s)(f))
    })
  }

  def fibs2(): Stream[Int] = {
    unfold((0, 1)) {
      case (f0, f1) => Some((f0, (f1, f0 + f1)))
    }
  }

  def from2(n: Int): Stream[Int] = {
    unfold(n)(s => Some((s, s + 1)))
  }

  def constant2[A](a: A): Stream[A] =
    unfold(a)(_ => Some((a, a)))

  def ones2(): Stream[Int] =
    unfold(1)(_ => Some((1, 1)))
}