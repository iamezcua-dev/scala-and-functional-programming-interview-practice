package com.rockthejvm.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)
  def apply(index: Int): T
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException()
  override def tail: RList[Nothing] = throw new NoSuchElementException()
  override def isEmpty: Boolean = true
  override def toString: String = "[]"
  override def apply(index: Int): Nothing = throw new NoSuchElementException()
}

case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if (remaining.isEmpty) result
      else if (remaining.tail.isEmpty) s"$result${remaining.head}"
      else toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }

    "[" + toStringTailrec(this, "") + "]"
  }

  override def apply(index: Int): T = {
    /*
    ## Desktop test:
      [1,2,3,4].apply(90) = retrieveIndex(0, [1,2,3,4])
      = retrieveIndex(1, [2,3,4])
      = retrieveIndex(2, [3,4])
      = retrieveIndex(3, [4])
      ===> The following `RNil.tail` will produce a NoSuchElementException and will stop the recursion:
      = retrieveIndex(4, [].tail)

     ## Algorithm complexity
     O(min(N, index)
     */
    @tailrec
    def applyTailrec(remainingList: RList[T], currentIndex: Int): T = {
      if(currentIndex == index) remainingList.head
      else applyTailrec(remainingList.tail, currentIndex + 1)
    }

    if (index < 0) throw new NoSuchElementException
    applyTailrec(this, 0)
  }

}

object ListProblems extends App {
  //  val aSmallList = Cons(1, Cons(2, Cons(3, RNil)))
  //  val aSmallList = ::(1, ::(2, ::(3, RNil)))
  val aSmallList = 1 :: 2 :: 3 :: 4 :: RNil
  println(aSmallList)

  println(aSmallList(0))
  println(aSmallList(1))
  println(aSmallList(2))
  println(aSmallList(3))
  println(aSmallList(90))
}
