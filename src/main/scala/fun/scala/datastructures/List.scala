package fun.scala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
	def apply[A](as: A*): List[A] = {
		if(as.isEmpty) Nil
		else Cons(as.head, apply(as.tail: _*))
	}

	def tail[A](as: List[A]): List[A] = as match {
		case Nil => Nil
		case Cons(h, t) => t
	}

	def setHead[A](a: A, as: List[A]): List[A] = as match {
		case Nil => as
		case Cons(h, t) => Cons(a, t)
	}

	def drop[A](l: List[A], n: Int): List[A] = l match {
		case Cons(h, t) => if(n == 0) l else drop(t, n - 1)
		case _ => l 
	}

	def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
		case Cons(h, t) if(f(h)) => dropWhile(t)(f)
		case _ => l
	}

	def init[A](l: List[A]): List[A] = l match {
		case Cons(h, ll @ Cons(hh, tt)) => Cons(h, init(ll))
		case _ => Nil 
	}

	def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = 
		l match {
			case Nil => z
			case Cons(h, t) => f(h, foldRight(t, z)(f)) 
		}
		
	def length[A](l: List[A]): Int = foldRight(l, 0)( (_, acc) => acc + 1)

	@annotation.tailrec
	def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = 
		l match {
			case Nil => z
			case Cons(h, t) => foldLeft(t, f(z, h))(f)
		}
		
}

