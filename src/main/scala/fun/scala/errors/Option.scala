package fun.scala.errors

import scala.{ Option => _ }

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def filter(p: A => Boolean): Option[A]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](default: => Option[B]): Option[B]
}

case object None extends Option[Nothing] {
  def map[B](f: Nothing => B): Option[B] = None
  def flatMap[B](f: Nothing => Option[B]): Option[B] = None
  def filter(p: Nothing => Boolean): Option[Nothing] = None
  def getOrElse[B >: Nothing](default: => B): B = default
  def orElse[B >: Nothing](default: => Option[B]): Option[B] = default
}

case class Some[+A](a: A) extends Option[A] {
  def map[B](f: A => B): Option[B] = Some(f(a))
  def flatMap[B](f: A => Option[B]): Option[B] = f(a)
  def filter(p: A => Boolean): Option[A] = if (p(a)) None else this
  def getOrElse[B >: A](default: => B): B = a
  def orElse[B >: A](default: => Option[B]): Option[B] = Some(a)
}

object Option {
  def variance(xs: Seq[Double]): Option[Double] = {
    val mean = if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

    mean.flatMap { m =>
      Some(xs.map(x => math.pow(x - m, 2.0)).sum / xs.length)
    }
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def sequence[A](as: List[Option[A]]): Option[List[A]] = as match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))
  }

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(as.map(f))
}