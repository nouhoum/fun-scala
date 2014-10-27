package fun.scala.errors

import scala.{ Option => _ }

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def filter(p: A => Boolean): Option[A]
  def getOrElse[B >: A](default: => B): B
  def getOrElse[B >: A](default: => Option[B]): Option[B]
}

case object None extends Option[Nothing] {
  def map[B](f: Nothing => B): Option[B] = None
  def flatMap[B](f: Nothing => Option[B]): Option[B] = None
  def filter(p: Nothing => Boolean): Option[Nothing] = None
  def getOrElse[B >: Nothing](default: => B): B = default
  def getOrElse[B >: Nothing](default: => Option[B]): Option[B] = default
}

case class Some[+A](a: A) extends Option[A] {
  def map[B](f: A => B): Option[B] = Some(f(a))
  def flatMap[B](f: A => Option[B]): Option[B] = f(a)
  def filter(p: A => Boolean): Option[A] = if (p(a)) None else this
  def getOrElse[B >: A](default: => B): B = a
  def getOrElse[B >: A](default: => Option[B]): Option[B] = Some(a)
}
