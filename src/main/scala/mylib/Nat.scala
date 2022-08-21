package Nat

// prove (a b : Nat) : a + b = b + a

sealed trait Nat {
  type :+[T <: Nat] <: Nat
  def +[T <: Nat](that: T): :+[T]

  def refl(): Equal[this.type,this.type]

  def commutativeZero(): Equal[this.type# :+[Zero.type], Zero.type# :+[this.type]]

  def commutativeSucc[M <: Nat](that: M): Equal[this.type# :+[Succ[M]], Succ[M]# :+[this.type]]

  def toInt(begin: Int = 0): Int = {
    this match {
      case Zero => begin
      case Succ(x) => x.toInt(begin+1)
    }
  }

}

final object Zero extends Nat {
  type :+[T <: Nat] = T
  override def +[T <: Nat](that: T) = that

  def refl(): Equal[Zero.type,Zero.type] = EquZ()

  def commutativeZero(): Equal[:+[Zero.type],:+[Zero.type]] = EquZ()

  def commutativeSucc[M <: Nat](that: M): Equal[:+[Succ[M]],Succ[M]# :+[Zero.type]] = EquS(that, that+Zero)

}

final case class Succ[T <: Nat](get: T) extends Nat {
  type :+[M <: Nat] = Succ[T# :+[M]]
  override def +[M <: Nat](that: M) = Succ(get + that)

  def refl(): Equal[this.type,this.type] = EquS(get,get)

  def commutativeZero(): Equal[this.type# :+[Zero.type],this.type] = EquS(get+Zero,get)

  def commutativeSucc[M <: Nat](that: M): Equal[this.type# :+[Succ[M]],Succ[M# :+[this.type]]] = EquS(get+Succ(that),that+this)

}

sealed trait Equal[-M <: Nat, -N <: Nat] {}
case class EquZ() extends Equal[Zero.type, Zero.type] {}
case class EquS[M <: Nat, N <: Nat](m: M, n: N) extends Equal[Succ[M],Succ[N]] {}

object test extends App {
  val two = Succ(Succ(Zero))
  val one = Succ(Zero)
  val three = two + one

  class UInt[T <: Nat](width: T) {
    def :=(that: UInt[T]): Unit = {
    }
  }
  val uone = new UInt(one)
  val utwo = new UInt(two)
  val utwo2 = new UInt(two)
  val uthree = new UInt(three)

  utwo := utwo2
  // uncomment the following line will get a type error
  // utwo := uone
  // uthree := utwo

  def check[T <: Nat](x: T)(y: T) = {}
  check(two+one)(one+two)

  println(three.toInt())

}
