package main.chapter3

sealed trait Tree[+A]

case class Leaf[A](a :A) extends Tree[A]

case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
   def size[A](t :Tree[A]): Int = t match {
      case null => 0
      case Leaf(x) => 1
      case Node(left, right) => 1 + size(left) + size(right)
   }

   def sizeF[A](t :Tree[A]): Int = fold(t)(a => 1)((l, r) => 1 + l + r)

   def max(t :Tree[Int]): Int = t match {
      case Leaf(x) => x
      case Node(left, right) => max(left) max max(right)
   }

   def maxF(t :Tree[Int]): Int = fold(t)(a => a)((a,b) => a.max(b))

   def depth[A](t :Tree[A]): Int = t match {
      case Leaf(x) => 0
      case Node(left, right) => 1 + (depth(left) max depth(right))
   }

   def depthF[A](t :Tree[A]): Int = fold(t)( a => 0)( (a, b) => 1 + a.max(b) )

   def map[A, B](t :Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(x) => Leaf(f(x))
      case Node(left, right) => Node(map(left)(f), map(right)(f))
   }

   def mapF[A, B](t :Tree[A])(f: A => B): Tree[B] = fold(t)(x => Leaf(f(x)) :Tree[B])((a, b) => Node(a, b))

   def fold[A , B](t :Tree[A])(f: A => B)(op: (B, B) => B) :B = t match {
      case Leaf(x) => f(x)
      case Node(l, r) => op( fold(l)(f)(op),
                              fold(r)(f)(op))
   }
}
