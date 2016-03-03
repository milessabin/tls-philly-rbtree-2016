package rbtree

import shapeless._

sealed trait Colour
case object Red extends Colour
case object Black extends Colour

sealed trait Tree[C <: Colour, N <: Nat, A]
case class E[A]() extends Tree[Black.type, _0, A]
case class TR[N <: Nat, A]
  (l: Tree[Black.type, N, A], r: Tree[Black.type, N, A], a: A) extends Tree[Red.type, N, A]
case class TB[N <: Nat, C1 <: Colour, C2 <: Colour, A]
  (l: Tree[C1, N, A], r: Tree[C2, N, A], a: A) extends Tree[Black.type, Succ[N], A]

object Tree {
  type RBT[A, N <: Nat] = Tree[Black.type, N, A]
}

trait Incr[C <: Colour, N <: Nat] {
  type Out <: Nat
}

object Incr {
  type Aux[C <: Colour, N <: Nat, Out0 <: Nat] =
    Incr[C, N] { type Out = Out0 }

  def apply[C <: Colour, N <: Nat](implicit incr: Incr[C, N]): Aux[C, N, incr.Out] = incr

  implicit def incrRed[N <: Nat]: Aux[Red.type, N, N] =
    new Incr[Red.type, N] {
      type Out = N
    }

  implicit def incrBlack[N <: Nat]: Aux[Black.type, N, Succ[N]] =
    new Incr[Black.type, N] {
      type Out = Succ[N]
    }
}

class AlmostTree[C <: Colour, N <: Nat, C1 <: Colour, C2 <: Colour, NC <: Nat, A]
  (l: Tree[C1, NC, A], r: Tree[C2, NC, A], a: A)

object AlmostTree {
  def apply[C <: Colour] = new Partial[C]
  class Partial[C <: Colour] {
    def apply[C1 <: Colour, C2 <: Colour, NC <: Nat, A, N <: Nat]
      (l: Tree[C1, NC, A], r: Tree[C2, NC, A], a: A)
      (implicit incr: Incr.Aux[C, NC, N]) = new AlmostTree[C, N, C1, C2, NC, A](l, r, a)
  }
}

object Demo {
  val t0 = E[Int]()
  val t1 = TB(E[Int](), E[Int](), 0)
  //val t2 = TB(t0, t1, 1)

  val t3 = AlmostTree[Black.type](t0, t0, 3)
}
