package mylib

sealed trait Bin {
  type This[C <: Bin] <: Bin
  type childType <: Bin
  type notType <: Bin
  type xorType[M <: Bin] <: Bin
  type andType[M <: Bin] <: Bin
  type add[M <: Bin] <: Bin
  def child: childType
  def father[C <: Bin](that: C): This[C]
  def not: notType
  def xor[M <: Bin](that: M): xorType[M]
  def and[M <: Bin](that: M): andType[M]
  def +[M <: Bin](that: M): add[M]
}

case object End extends Bin {
  type This[C <: Bin] = End.type
  type childType = End.type
  type notType = One[End.type]
  type xorType[M <: Bin] = M
  type andType[T <: Bin] = Zero[T# childType]
  type add[T <: Bin] = T
  override def child = End
  override def father[C <: Bin](that: C) = End
  override def not = One(End)
  override def xor[M <: Bin](that: M) = that
  override def and[M <: Bin](that: M) = Zero(that.child)
  override def +[T <: Bin](that: T) = {
    that
  }
}

case class Zero[T <: Bin](get: T) extends Bin {
  type This[C <: Bin] = Zero[C]
  type childType = T
  type notType = One[T]
  type xorType[M <: Bin] = M
  type andType[M <: Bin] = Zero[M# childType]
  type add[M <: Bin] = M# This[get.add[M# childType]]
  override def child = get
  override def father[C <: Bin](that: C) = Zero(that)
  override def not = One(get)
  override def xor[M <: Bin](that: M) = that
  override def and[M <: Bin](that: M) = Zero(that.child)
  override def +[M <: Bin](x: M) = {
    this.xor(x).father(get + (x.child))
  }
}
 
case class One[T <: Bin](get: T) extends Bin {
  type This[C <: Bin] = One[C]
  type childType = T
  type notType = Zero[T]
  type xorType[M <: Bin] = M#notType
  type andType[M <: Bin] = M
  type add[M <: Bin] = M#notType#This[M#This[End.type]#add[T]#add[M#childType]]//
  override def child = get
  override def father[C <: Bin](that: C) = One(that)
  override def not = Zero(get)
  override def xor[M <: Bin](that: M) = that.not
  override def and[M <: Bin](that: M) = that
  override def +[M <: Bin](that: M) = {
    that.not.father(that.father(End) + get + that.child)//
  }
}

object bintest extends App {
  val one = One(Zero(End))
  val two = Zero(One(End))
  val three = one + two
  val two2 = one + one
  val four = two + two

  def check[T <: Bin](a: T)(b: T) = {}
  check( One(One(End)) )(one+two)

  // check( Zero(One(End)) )(one+two)

}
