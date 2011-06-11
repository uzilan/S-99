/*
solutions to S-99: Ninety-Nine Scala Problems
http://aperiodic.net/phil/scala/s-99/
Working with lists
*/

import scala.util.Random

object WorkingWithLists {

	/*
	P01 (*) Find the last element of a list.
	
	scala> last(List(1, 1, 2, 3, 5, 8))
	res0: Int = 8
	*/
	def last[A](l: List[A]): A = l.last
	
	/*
	P02 (*) Find the last but one element of a list.
	
	scala> penultimate(List(1, 1, 2, 3, 5, 8))
	res0: Int = 5
	*/
	def penultimate[A](l: List[A]) : A = l.init.last
	
	/*
	P03 (*) Find the Kth element of a list.
	By convention, the first element in the list is element 0.
	
	scala> nth(2, List(1, 1, 2, 3, 5, 8))
	res0: Int = 2
	*/
	def nth[A](i: Int, l: List[A]): A = l(i)
	
	/*
	P04 (*) Find the number of elements of a list.
	
	scala> length(List(1, 1, 2, 3, 5, 8))
	res0: Int = 6
	*/
	def length[A](l: List[A]): Int = l.size
	
	/*
	P05 (*) Reverse a list.
	
	scala> reverse(List(1, 1, 2, 3, 5, 8))
	res0: List[Int] = List(8, 5, 3, 2, 1, 1)
	*/
	def reverse[A](l: List[A]): List[A] = l.reverse
	
	/*
	P06 (*) Find out whether a list is a palindrome.
	scala> isPalindrome(List(1, 2, 3, 2, 1))
	res0: Boolean = true
	*/
	def isPalindrome[A](l: List[A]): Boolean = l == l.reverse

	/*
	P07 (**) Flatten a nested list structure.
	
	scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
	res0: List[Any] = List(1, 1, 2, 3, 5, 8)
	*/
	def flatten(l: List[Any]): List[Any] = l flatMap {
		case ms: List[_] => flatten(ms)
		case e => List(e)
	  }
	
	/*
	P08 (**) Eliminate consecutive duplicates of list elements.
	If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
	
	scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
	*/
	def compress[A](l: List[A]): List[A] = {	
		l.foldRight(List[A]()) {
			(x, y) => if (y.isEmpty || y.head != x) x :: y 
			else y
		}
	}
	
	/*
	P09 (**) Pack consecutive duplicates of list elements into sublists.
	If a list contains repeated elements they should be placed in separate sublists.
	
	scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
	*/
	def pack[A](l: List[A]): List[List[A]] = {
		if (l.isEmpty) List(List())
		else {
			val (packed, next) = l span { _ == l.head }
			if (next == Nil) List(packed)
			else packed :: pack(next)
		}
	}
	
	/*
	P10 (*) Run-length encoding of a list.
	Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
	
	scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
	*/
	def encode[A](l: List[A]): List[(Int, A)] = {
		pack(l).map(x => (x.length, x.head))
	}
	
	/*
	P11 (*) Modified run-length encoding.
	Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
	
	scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
	*/
	def encodeModified[A](l: List[A]): List[Any] = {
		encode(l).map(x => {
			if (x._1 == 1) x._2
			else x
		})
	}
	
	/*
	P12 (**) Decode a run-length encoded list.
	Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
	
	scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
	res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
	*/
	def decode[A](l: List[(Int, A)]): List[A] = {
		l flatMap { e => List.fill(e._1)(e._2) }
	}
	
	/*
	P13 (**) Run-length encoding of a list (direct solution).
	Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
	
	scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
	*/
	def encodeDirect[A](l: List[A]): List[(Int, A)] = {
		if (l.isEmpty) Nil
		else {
		  val (packed, next) = l span { _ == l.head }
		  (packed.length, packed.head) :: encodeDirect(next)
		}
	}
	
	/*
	P14 (*) Duplicate the elements of a list.
	
	scala> duplicate(List('a, 'b, 'c, 'c, 'd))
	res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
	*/
	def duplicate[A](l: List[A]): List[A] = {
		l flatMap { x => List(x, x) }
	}
	
	/*
	P15 (**) Duplicate the elements of a list a given number of times.
	
	scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
	res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
	*/
	def duplicateN[A](n: Int, l: List[A]): List[A] = {
		l flatMap { List.fill(n)(_) } 
	}
	
	/*
	P16 (**) Drop every Nth element from a list.
	
	scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
	*/
	def drop[A](n: Int, l: List[A]): List[A] = {
		l.zipWithIndex filter {
			x => (x._2 + 1) % n != 0
		} map {_._1}
	}
	
	/*
	P17 (*) Split a list into two parts.
	The length of the first part is given. Use a Tuple for your result.
	
	scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	*/
	def split[A](n: Int, l: List[A]): (List[A], List[A]) = {
		l splitAt n
	}
	
	/*
	P18 (**) Extract a slice from a list.
	Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
	
	scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	res0: List[Symbol] = List('d, 'e, 'f, 'g)
	*/
	def slice[A](f: Int, t: Int, l: List[A]): List[A] = {
		l.slice(f, t)
	}
	
	/*
	P19 (**) Rotate a list N places to the left.
	
	scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

	scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
	*/
	def rotate[A](n: Int, l: List[A]): List[A] = {
		if (n > 0) (l drop n) ::: (l take n)
		else (l drop l.length + n) ::: (l take l.length + n)
	}
	
	/* 
	P20 (*) Remove the Kth element from a list.
	Return the list and the removed element in a Tuple. Elements are numbered from 0.

	scala> removeAt(1, List('a, 'b, 'c, 'd))
	res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
	*/
	def removeAt[A](n: Int, l: List[A]): (List[A], A) = {
		((l take n) ::: (l drop n + 1), l(n))
	}
	
	/*
	P21 (*) Insert an element at a given position into a list.
	
	scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
	res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
	*/
	def insertAt[A](e: A, n: Int, l: List[A]) = {
		l.splitAt(n) match {
			case (pre, post) => pre ::: e :: post
		}
	}
	
	/*
	P22 (*) Create a list containing all integers within a given range.

	scala> range(4, 9)
	res0: List[Int] = List(4, 5, 6, 7, 8, 9)
	*/
	def range(f: Int, t: Int): List[Int] = {
		List.range(f, t + 1)
	}
	
	/*
	P23 (**) Extract a given number of randomly selected elements from a list.

	scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
	res0: List[Symbol] = List('e, 'd, 'a)
	Hint: Use the solution to problem P20
	*/
	def randomSelect[A](n: Int, l: List[A]): List[A] = {
		if (n <= 0) Nil
		else {
			val (rest, e) = removeAt((new util.Random).nextInt(l.length), l)
			e :: randomSelect(n - 1, rest)
		}
	}
	
	/*
	P24 (*) Lotto: Draw N different random numbers from the set 1..M.

	scala> lotto(6, 49)
	res0: List[Int] = List(23, 1, 17, 33, 21, 37)
	*/
	def lotto(n: Int, max: Int): List[Int] = {
		randomSelect(n, List.range(1, max + 1))
	}
}




















