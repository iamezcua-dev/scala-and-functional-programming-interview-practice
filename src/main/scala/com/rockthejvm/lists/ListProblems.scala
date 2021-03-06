package com.rockthejvm.lists

import scala.annotation.tailrec

sealed abstract class RList[+T] {
  def head: T
  def tail: RList[T]
  def isEmpty: Boolean

  /**
   * Appends the <code>elem</code> at the beginning of the list.
   * @param elem The element to prepend to the list from the caller.
   * @tparam S The type of the elements the list will contain.
   * @return A list containing <code>elem</code> prepended to the caller's list.
   */
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)
  def apply(index: Int): T
  def length: Int
  def reverse: RList[T]
  // concatenate another list to this one
  def ++[S >: T](anotherList: RList[S]): RList[S]
}

case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException()
  override def tail: RList[Nothing] = throw new NoSuchElementException()
  override def isEmpty: Boolean = true
  override def toString: String = "[]"
  override def apply(index: Int): Nothing = throw new NoSuchElementException()
  override def length: Int = 0
  override def reverse: RList[Nothing] = RNil
  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList
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

  override def length: Int = {
    /*
    * List can be:
    *   - Of size greater than zero (e.g. 1, 4, 99, 20000, etc.)
    *   - Of size zero (empty list)
    *
    * Algorithm complexity:
    *   - O(n), since we don't know the list size in advance and we would need to traverse it entirely.
    */
    @tailrec
    def countItems(itemCount: Int, remainingList: RList[T]): Int = {
      if( remainingList.isEmpty) itemCount
      else countItems(itemCount+1, remainingList.tail)
    }

    countItems(0, this)
  }

  override def reverse: RList[T] = {
    /*
    Desktop test:

    [1,2,3,4].reverse = reverse.([1,2,3,4], RNil)
    = reverse([2, 3, 4], [1])
    = reverse([3, 4], [2, 1])
    = reverse([4], [3, 2, 1])
    = reverse([], [4, 3, 2, 1])
    = [4, 3, 2, 1]

    Algorithm complexity = O(N), since we need to traverse all the elements.
     */
    @tailrec
    def reverse(remainingList: RList[T], reversedList: RList[T]): RList[T] = {
      if (remainingList.isEmpty) reversedList
      else reverse(remainingList.tail, remainingList.head :: reversedList)
    }

    reverse(this, RNil)
  }

  /**
   * Appends another list to the caller's list in order.
   *
   * - Algorithm complexity: O(2M)
   * - Rationale: Assuming that M is the caller's list and N is the list to concatenate, then N would need a previous
   *    reverse operation, for after prepending each and every of its elements to M.
   *
   * @param anotherList The list to concatenate.
   * @tparam S The type of the elements on the list.
   * @return A list containing the elements of the caller's list plus the elements in the <code>anotherList</code>.
   */
  override def ++[S >: T](anotherList: RList[S]): RList[S] = {
    @tailrec
    def concatenateHelper(accumulator: RList[S], remainingElements: RList[S]): RList[S] = {
      if(remainingElements.isEmpty) accumulator
      else concatenateHelper( remainingElements.head :: accumulator, remainingElements.tail)
    }

    concatenateHelper(anotherList, this.reverse)
  }
}

object RList {
  def from[T](it: Iterable[T]): RList[T] = {
    @tailrec
    def convertToList(remaining: Iterable[T], accumulator: RList[T]): RList[T] = {
      if(remaining.isEmpty) accumulator
      else convertToList(remaining.tail, remaining.head :: accumulator)
    }

    convertToList(it, RNil).reverse
  }
}

object ListProblems extends App {
  //  val aSmallList = Cons(1, Cons(2, Cons(3, RNil)))
  //  val aSmallList = ::(1, ::(2, ::(3, RNil)))
  val aSmallList = 1 :: 2 :: 3 :: 4 :: RNil
  val aLargeList = RList.from(1 to 10000)
  println(aSmallList)
  println(aLargeList)

  println(aSmallList(0))
  println(aSmallList(1))
  println(aSmallList(2))
  println(aSmallList(3))
  val expression: Int = try { aSmallList(90) } catch { case n: NoSuchElementException => -1}
  println(expression)
  println(aLargeList(8735))

  println(s"The our list contains ${aSmallList.length} elements")
  println(s"An empty list contains ${RNil.length} elements")
  println(s"Our large list contains ${aLargeList.length} elements.")

  println(aSmallList.reverse)
  println(aLargeList.reverse)

  val anotherSmallList = 5 :: 6 :: 7 :: 8 :: 9 :: RNil
  println(s"Another small list: $anotherSmallList")

  println(s"Small list ++ another small list: ${aSmallList ++ anotherSmallList}")
}
