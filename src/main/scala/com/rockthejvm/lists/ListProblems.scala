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

  // remove an element at a given index, return a NEW list
  def removeAt(index: Int): RList[T]

  // the big three
  def map[S](f: T => S): RList[S]
  def flatMap[S](f: T => RList[S]): RList[S]
  def filter(f: T => Boolean): RList[T]
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


case object RNil extends RList[Nothing] {
  override def head: Nothing = throw new NoSuchElementException()
  override def tail: RList[Nothing] = throw new NoSuchElementException()
  override def isEmpty: Boolean = true
  override def toString: String = "[]"
  override def apply(index: Int): Nothing = throw new NoSuchElementException()
  override def length: Int = 0
  override def reverse: RList[Nothing] = RNil
  override def ++[S >: Nothing](anotherList: RList[S]): RList[S] = anotherList
  override def removeAt(index: Int): RList[Nothing] = RNil
  override def map[S](f: Nothing => S): RList[S] = RNil
  override def flatMap[S](f: Nothing => RList[S]): RList[S] = RNil
  override def filter(f: Nothing => Boolean): RList[Nothing] = RNil
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


  /**
   * Remove the element at the provided index.
   *
   * - Algorithm Complexity:
   *    - O(1), for the best case
   *    - O(n + m) for the worst case
   * - Rationale:
   *    - If the element you want to remove is the first element, you just drop it and return the tail of the list.
   *    - Else, we would need to traverse the list of size N and when the element is dropped, we would need to append
   *      the remainingList to a reversed version of the accumulatorList of size M.
   *
   * @param index The index of the element to remove from the list.
   * @return
   */
  override def removeAt(index: Int): RList[T] = {
    @tailrec
    def removeAtHelper(remainingList: RList[T], accumulatorList: RList[T] = RNil, currentIndex: Int = 0): RList[T] = {
      if(currentIndex == index) accumulatorList.reverse ++ remainingList.tail
      else removeAtHelper(remainingList.tail, remainingList.head :: accumulatorList  , currentIndex + 1)
    }

    if(index >= this.length) throw new IndexOutOfBoundsException
    else removeAtHelper(this)
  }

  /**
   *
   * @param f
   * @tparam S
   * @return
   */
  override def map[S](f: T => S): RList[S] = {
    /*
      t = (x: x + 1)
      [1,2,3].map(transformer) = mapHelper([2,3], [t(1)])
      = mapHelper([3], [t(2), t(1)])
      = mapHelper([], [t(3), t(2), t(1)])
      [t(3), t(2), t(1)].reverse
     */
    @tailrec
    def mapHelper(remainingElements: RList[T], mappedElements: RList[S]): RList[S] = {
      if (remainingElements.length <= 0) mappedElements
      else {
        val transformedValue: S = f(remainingElements.head)
        mapHelper(remainingElements.tail, transformedValue :: mappedElements)
      }
    }

    mapHelper(this, RNil).reverse
  }

  /**
   *
   * @param f
   * @tparam S
   * @return
   */
  override def flatMap[S](f: T => RList[S]): RList[S] = {
    /*
      val t = (i: T) => i :: (i*3) :: RNil

      [1,2,3,4].flatMap(t) = flatMapHelper([1,2,3,4], [])
      = flatMapHelper([2,3,4], [[1, 3].reverse])
      = flatMapHelper([3,4], [[2, 6].reverse], [3, 1])
      = flatMapHelper([4], [[3, 9].reverse, [6, 2], [3, 1]])
      = flatMapHelper([], [[4, 12].reverse, [9, 3], [6, 2], [3, 1]])
      = [[12, 4], [9, 3], [6, 2], [3, 1]].flatten = [12, 4, 9, 3, 6, 2, 3, 1].reverse
      = [1, 3, 2, 6, 3, 9, 4, 12]
     */
    //    def flatten: RList[T] = {
    //      @tailrec
    //      def flattenHelper(remainingElements: RList[RList[T]], flattenedList: RList[T]): RList[T] = {
    //        if(remainingElements.length <= 0 ) flattenedList
    //        else {
    //          val head: RList[T] = remainingElements.head
    //          flattenHelper(remainingElements.tail, head ++ flattenedList)
    //        }
    //      }
    //
    //      flattenHelper(this, RNil).reverse
    //    }
    //
    //    val mappedList = this.map(f)
    //    val flattenedList = flatten(mappedList)
    ???
  }

  /**
   *
   * @param f
   * @return
   */
  override def filter(f: T => Boolean): RList[T] = {
    ???
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

  val myList: RList[Int] = aSmallList ++ anotherSmallList
  println(s"Small list ++ another small list (myList): ${myList}")

  println(s"Dropping the 1st element from myList: ${myList.removeAt(0)}")
  println(s"Dropping the 1st and 3rd element from myList: ${myList.removeAt(0).removeAt(2)}")
  println(s"Dropping the 5th element from myList: ${myList.removeAt(4)}")

  /*
    map
   */
  val transformer = (i: Int) => i * 2
  println(s"Duplicating every number in the list: ${myList.map(transformer)}")

  /*
    flatten
   */
  val myList2 = (1 :: 3 :: RNil) :: (2 :: 6 :: RNil) :: (3 :: 9 :: RNil) :: (4 :: 12 :: RNil) :: RNil
  println(s"Original list ${myList2}")
  println(s"The flattened version of the list ${myList2.flatten}")

  /*
    flatMap
   */
  val numberAndItsTriple = (i: Int) => i :: (i*3) :: RNil
  println(s"The even numbers in the list are: ${myList.flatMap(numberAndItsTriple)}")

  /*
    filter
   */
  val evenNumbers = (i: Int) => i % 2 == 0
  println(s"The even numbers in the list are: ${myList.filter(evenNumbers)}")
}
