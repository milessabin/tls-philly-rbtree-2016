package rbtree

import Ordering.Implicits._
import shapeless._

sealed trait AlmostTree[N <: Nat, A] {
  def balanceLB(z: A, d: Tree[N, A]): AlmostTree[Succ[N], A]
  def balanceLR(z: A, d: Tree[N, A]): AlmostTree[N, A]
  def balanceRB(a: Tree[N, A], x: A): AlmostTree[Succ[N], A]
  def balanceRR(a: Tree[N, A], x: A): AlmostTree[N, A]
  def blacken: RBT[A]
}

sealed trait Colour {
  type Incr[N <: Nat] <: Nat

  def balanceL[A, N <: Nat](l: AlmostTree[N, A], z: A, d: Tree[N, A]): AlmostTree[Incr[N], A]
  def balanceR[A, N <: Nat](a: Tree[N, A], x: A, r: AlmostTree[N, A]): AlmostTree[Incr[N], A]
}

case object Red extends Colour {
  type Incr[N <: Nat] = N

  case class AT[N <: Nat, A](l: Tree[N, A], a: A, r: Tree[N, A]) extends AlmostTree[Incr[N], A] {
    def balanceLB(z: A, d: Tree[N, A]): AlmostTree[Succ[N], A] =
      (l, a, r) match {
        case (TR(a, x, b), y, c) => Red.AT(TB(a, x, b), y, TB(c, z, d))
        case (a, x, TR(b, y, c)) => Red.AT(TB(a, x, b), y, TB(c, z, d))
        case (a: TreeB[_, _], x, b: TreeB[_, _]) => Black.AT(TR(a, x, b), z, d)
      }

    def balanceLR(z: A, d: Tree[N, A]): AlmostTree[N, A] =
      (l, a, r) match {
        case (a: TreeB[_, _], x, b: TreeB[_, _]) => Red.AT[N, A](TR(a, x, b), z, d)
      }

    def balanceRB(a: Tree[N, A], x: A): AlmostTree[Succ[N], A] =
      (l, this.a, r) match {
        case (TR(b, y, c), z, d) => Red.AT(TB(a, x, b), y, TB(c, z, d))
        case (b, y, TR(c, z, d)) => Red.AT(TB(a, x, b), y, TB(c, z, d))
        case (b: TreeB[_, _], z, d: TreeB[_, _]) => Black.AT(a, x, TR(b, z, d))
      }

    def balanceRR(a: Tree[N, A], x: A): AlmostTree[N, A] =
      (l, this.a, r) match {
        case (b: TreeB[_, _], z, d: TreeB[_, _]) => Red.AT[N, A](a, x, TR(b, z, d))
      }

    def blacken: RBT[A] = Root(TB(l, a, r))
  }

  def balanceL[A, N <: Nat](l: AlmostTree[N, A], z: A, d: Tree[N, A]): AlmostTree[Incr[N], A] =
    l.balanceLR(z, d)

  def balanceR[A, N <: Nat](a: Tree[N, A], x: A, r: AlmostTree[N, A]): AlmostTree[Incr[N], A] =
    r.balanceRR(a, x)
}

case object Black extends Colour {
  type Incr[N <: Nat] = Succ[N]

  case class AT[N <: Nat, A](l: Tree[N, A], a: A, r: Tree[N, A]) extends AlmostTree[Incr[N], A] {
    def balanceLB(z: A, d: Tree[Succ[N], A]): AlmostTree[Succ[Succ[N]], A] =
      Black.AT(TB(l, a, r), z, d)

    def balanceLR(z: A, d: Tree[Succ[N], A]): AlmostTree[Succ[N], A] =
      Red.AT(TB(l, a, r), z, d)

    def balanceRB(a: Tree[Succ[N], A], x: A): AlmostTree[Succ[Succ[N]], A] =
      Black.AT(a, x, TB(l, this.a, r))

    def balanceRR(a: Tree[Succ[N], A], x: A): AlmostTree[Succ[N], A] =
      Red.AT(a, x, TB(l, this.a, r))

    def blacken: RBT[A] = Root(TB(l, a, r))
  }

  //balanceL :: Sing c -> AlmostTree n a -> a -> Tree n c1 a -> AlmostTree (Incr c n) a
  //balanceL SB (AT SR (TR a x b) y c) z d = AT SR (TB a x b) y (TB c z d)
  //balanceL SB (AT SR a x (TR b y c)) z d = AT SR (TB a x b) y (TB c z d)
  //balanceL c (AT SB a x b) z d             = AT c (TB a x b) z d
  //balanceL c (AT SR a@(TB _ _ _) x b@(TB _ _ _)) z d = AT c (TR a x b) z d
  //balanceL c (AT SR a@E x b@E) z d       = AT c (TR a x b) z d
  def balanceL[A, N <: Nat](l: AlmostTree[N, A], z: A, d: Tree[N, A]): AlmostTree[Succ[N], A] =
    l.balanceLB(z, d)

  //balanceR :: Sing c -> Tree n c1 a -> a -> AlmostTree n a -> AlmostTree (Incr c n) a
  //balanceR SB a x (AT SR (TR b y c) z d) = AT SR (TB a x b) y (TB c z d)
  //balanceR SB a x (AT SR b y (TR c z d)) = AT SR (TB a x b) y (TB c z d)
  //balanceR c a x (AT SB b z d)           = AT c a x (TB b z d)
  //balanceR c a x (AT SR b@(TB _ _ _) z d@(TB _ _ _)) = AT c a x (TR b z d)
  //balanceR c a x (AT SR b@E z d@E)       = AT c a x (TR b z d)
  def balanceR[A, N <: Nat](a: Tree[N, A], x: A, r: AlmostTree[N, A]): AlmostTree[Incr[N], A] =
    r.balanceRB(a, x)
}

sealed trait Tree[N <: Nat, A] {
  type C <: Colour
  val c: C
}

object Tree {
  type Aux[C0 <: Colour, N <: Nat, A] = Tree[N, A] { type C = C0 }

  //insert :: (Ord a) => a -> RBT a -> RBT a
  //insert x (Root s) = blacken (ins x s)
  def insert[A: Ordering](x: A, r: RBT[A]): RBT[A] =
    r match {
      case Root(s) => ins(x, s).blacken
    }

  //ins :: Ord a => a -> Tree n c a -> AlmostTree n a
  //ins x E = AT SR E x E
  //ins x s@(TR a y b) | x < y     = balanceL SR (ins x a) y b
  //                   | x > y     = balanceR SR a y (ins x b)
  //                   | otherwise = (AT SR a y b)
  //ins x s@(TB a y b) | x < y     = balanceL SB (ins x a) y b
  //                   | x > y     = balanceR SB a y (ins x b)
  //                   | otherwise = (AT SB a y b)
  def ins[N <: Nat, A: Ordering](x: A, t: Tree[N, A]): AlmostTree[N, A] =
    t match {
      case E()                  => Red.AT(E(), x, E())
      case TR(a, y, b) if x < y => balanceL(Red, ins(x, a), y, b)
      case TR(a, y, b) if x > y => balanceR(Red, a, y, ins(x, b))
      case TR(a, y, b)          => Red.AT[N, A](a, y, b)
      case TB(a, y, b) if x < y => balanceL(Black, ins(x, a), y, b)
      case TB(a, y, b) if x > y => balanceR(Black, a, y, ins(x, b))
      case TB(a, y, b)          => Black.AT(a, y, b)
    }

  //balanceL :: Sing c -> AlmostTree n a -> a -> Tree n c1 a -> AlmostTree (Incr c n) a
  def balanceL[A, N <: Nat](c: Colour, l: AlmostTree[N, A], z: A, d: Tree[N, A]): AlmostTree[c.Incr[N], A] =
    c.balanceL(l, z, d)

  //balanceR :: Sing c -> Tree n c1 a -> a -> AlmostTree n a -> AlmostTree (Incr c n) a
  def balanceR[A, N <: Nat](c: Colour, a: Tree[N, A], x: A, r: AlmostTree[N, A]): AlmostTree[c.Incr[N], A] =
    c.balanceR(a, x, r)
}

trait BalanceL[C0 <: Colour, C1 <: Colour, N <: Nat, A] {
  def balanceL(l: AlmostTree[N, A], z: A, d: Tree[N, A]): AlmostTree[C0#Incr[N], A]
}

case class TR[N <: Nat, A]
  (l: Tree.Aux[Black.type, N, A], a: A, r: Tree.Aux[Black.type, N, A]) extends Tree[N, A] {
  type C = Red.type
  val c = Red
}

sealed trait TreeB[N <: Nat, A] extends Tree[N, A] {
  type C = Black.type
  val c = Black
}

case class TB[N <: Nat, A](l: Tree[N, A], a: A, r: Tree[N, A]) extends TreeB[Succ[N], A]
case class E[A]() extends TreeB[_0, A]

sealed trait RBT[A]
case class Root[N <: Nat, A](t: TreeB[N, A]) extends RBT[A]

object Demo {
  val t0 = E[Int]()
  val t1 = TB(E[Int](), 0, E[Int]())
  val rbt1 = Root(t1)
  //val t2 = TB(t0, 1, t1)

  val t3: AlmostTree[Succ[_0], Int] = Black.AT(t0, 3, t0)
  val t4: AlmostTree[_0, Int] = Red.AT(t0, 3, t0)
  val t5: AlmostTree[Succ[Succ[_0]], Int] = Black.AT(t1, 4, t1)
  val t6: AlmostTree[Succ[_0], Int] = Red.AT(t1, 4, t1)
}
