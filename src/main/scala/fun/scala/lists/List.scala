package fun.scala.lists

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Cons(_, t) => t
    case _ => throw new NoSuchElementException
  }

  def setHead[A](h: A, as: List[A]): List[A] = as match {
    case Cons(_, tail) => Cons(h, tail)
    case Nil => throw new NoSuchElementException
  }

  def drop[A](l: List[A], n: Int): List[A] = (n, l) match {
    case (_, Nil) | (0, _) => l
    case (m, Cons(h, t)) => drop(t, m - 1)
  }

  def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
    case Cons(h, t) if p(h) => dropWhile(t, p)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(l: List[A], acc: B): B = {
      l match {
        case Nil => acc
        case Cons(h, t) => go(t, f(acc, h))
      }
    }

    go(as, z)
  }

  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) => b)(
      (a, g) => b => g(f(b, a))
    )(z)

  //foldRight(as, z)((a, b) => f(b, a))

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def sum(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product(ns: List[Int]) = foldLeft(ns, 1)(_ * _)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, e) => Cons(e, acc))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] =
    foldLeft(l, r)((acc, h) => Cons(h, acc))

  def concatenate[A](l: List[List[A]]) =
    foldLeft(l, Nil: List[A])(appendViaFoldLeft)

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def map2[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h, acc) => Cons(f(h), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, acc) => if (f(h)) Cons(h, acc) else acc)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((h, acc) => appendViaFoldLeft(f(h), acc))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(h => if (f(h)) List(h) else Nil)

  def corresponding(l: List[Int], r: List[Int]): List[Int] =
    (l, r) match {
      case (Cons(h0, t0), Cons(h1, t1)) => Cons(h0 + h1, corresponding(t0, t1))
      case _ => Nil
    }

  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] =
    (l, r) match {
      case (Cons(h0, t0), Cons(h1, t1)) => Cons(f(h0, h1), zipWith(t0, t1)(f))
      case _ => Nil
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    ???
  }
}