/**
  * Created by tamamu on 5/27/16.
  */
package datastructures

sealed trait List2[+A]

case object Nil extends List2[Nothing]

case class Cons[+A](head: A, tail: List2[A]) extends List2[A]

object List2 {
  def sum(ints: List2[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List2[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List2[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // リストの先頭要素を削除
  def tail[A](l: List2[A]): List2[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(_, t) => t
  }

  // リストの先頭を置き換える
  def setHead[A](l: List2[A], x: A): List2[A] = l match {
    case Nil => List2(x)
    case Cons(_, t) => Cons(x, t)
  }

  // リストの先頭からn個削除
  def drop[A](l: List2[A], n: Int): List2[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  // 関数fを満たす要素を先頭から削除していく
  /*
  def dropWhile[A](l: List2[A], f: A => Boolean): List2[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) dropWhile(t, f)
      else l
  }
  */
  def dropWhile[A](as: List2[A])(f: A => Boolean): List2[A] =
    as match {
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => as
    }

  def append[A](a1: List2[A], a2: List2[A]): List2[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List2[A]): List2[A] =
    l match {
      case Nil => l
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def foldRight[A, B](as: List2[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List2[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List2[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List2[A]): Int =
    foldRight(as, 0)((x, y) => y + 1)

  /*
  def foldLeft[A,B](as: List2[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(foldLeft(xs, z)(f), z))
    }
   */
  def foldLeft[A, B](as: List2[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum2l(ns: List2[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product2l(ns: List2[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def lengthL[A](as: List2[A]): Int =
    foldLeft(as, 0)((y, x) => y + 1)

  // foldRight で reverse を実装
  def reverseRight[A](as: List2[A]): List2[A] =
    foldRight(as, Nil: List2[A])((x, y) => append(y, List2(x)))

  // foldLeft で reverse を実装
  def reverseLeft[A](as: List2[A]): List2[A] =
    foldLeft(as, Nil: List2[A])((x, y) => Cons(y, x))

  // foldRight を foldLeft で実装
  // スタックセーフになる
  def foldRightLeft[A, B](as: List2[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverseLeft(as), z)((x, y) => f(y, x))

  // foldLeft を foldRight で実装
  def foldLeftRight[A, B](as: List2[A], z: B)(f: (B, A) => B): B =
    foldRight(reverseRight(as), z)((x, y) => f(y, x))

  // append を foldRight で実装
  def appendRight[A](a1: List2[A], a2: List2[A]): List2[A] =
    foldRight(a1, a2)((x, y) => Cons(x, y))

  // 複数のリストからなるリストを1つのリストとして連結
  def flatten[A](as: List2[List2[A]]): List2[A] =
    foldRight(as, Nil: List2[A])((x, y) => append(x, y))

  // 各要素に1を足したリストを返す
  def plusOne(xs: List2[Int]): List2[Int] =
    foldRight(xs, Nil: List2[Int])((x, y) => Cons(x + 1, y))

  // Double型要素をString型要素に変換したリストを返す
  def doubleToString(ds: List2[Double]): List2[String] =
    foldRight(ds, Nil: List2[String])((x, y) => Cons(x.toString, y))

  // リストの各要素を変更し、かつリストの構造をそのまま保つ総称関数
  def map[A, B](as: List2[A])(f: A => B): List2[B] =
    foldRight(as, Nil: List2[B])((x, y) => Cons(f(x), y))

  // 与えられた述語条件が満たされるまでリストから要素を削除する
  def filter[A](as: List2[A])(f: A => Boolean): List2[A] =
    as match {
      case Nil => as
      case Cons(x, y) => if (f(x)) filter(y)(f) else Cons(x, filter(y)(f))
    }

  def flatMap[A, B](as: List2[A])(f: A => List2[B]): List2[B] =
    flatten(map(as)(f))

  // filter を flatMap で実装
  def filterMap[A](as: List2[A])(f: A => Boolean): List2[A] =
    flatMap(as)((x) => if (f(x)) Nil else List2(x))

  def plusList(a1: List2[Int], a2: List2[Int]): List2[Int] =
    a1 match {
      case Nil => Nil
      case Cons(x1, y1) => a2 match {
        case Nil => Nil
        case Cons(x2, y2) => Cons(x1 + x2, plusList(y1, y2))
      }
    }

  // plusListを一般化
  def zipWith[A](a1: List2[A], a2: List2[A])(f: (A, A) => A): List2[A] =
    a1 match {
      case Nil => Nil
      case Cons(x1, y1) => a2 match {
        case Nil => Nil
        case Cons(x2, y2) => Cons(f(x1, x2), zipWith(y1, y2)(f))
      }
    }
}
