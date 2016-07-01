package datastructures

/**
  * Created by tamamu on 7/1/16.
  */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](root: Tree[A]): Int =
    root match {
      case Leaf(v) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  def maximum(root: Tree[Int]): Int =
    root match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  def depth[A](root: Tree[A]): Int =
    root match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

  def map[A](root: Tree[A], f: A => A): Tree[A] =
    root match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l, f), map(r, f))
    }
}