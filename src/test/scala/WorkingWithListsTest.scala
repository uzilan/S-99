import org.scalatest.Spec

import WorkingWithLists._

class WorkingWithListsTest extends Spec {

	// P1 - last
	expect(8) {
		last(List(1, 1, 2, 3, 5, 8))
	}
	
	// P2 - penultimate
	expect(5) {
		penultimate(List(1, 1, 2, 3, 5, 8))
	}
	
	// P3 - nth
	expect(2) {
		nth(2, List(1, 1, 2, 3, 5, 8))
	}
	
	// P4 - length
	expect(6) {
		length(List(1, 1, 2, 3, 5, 8))
	}
	
	// P5 - reverse
	expect(List(8, 5, 3, 2, 1, 1)) {
		reverse(List(1, 1, 2, 3, 5, 8))
	}
	
	// P6 - isPalindrome
	expect(true) {
		isPalindrome(List(1, 2, 3, 2, 1))
	}
	
	// P6 - isPalindrome
	expect(false) {
		isPalindrome(List(1, 2, 3, 2, 10))
	}
	
	// P7 - flatten
	expect(List(1, 1, 2, 3, 5, 8)) {
		flatten(List(List(1, 1), 2, List(3, List(5, 8))))
	}
	
	// P8 - compress
	expect(List('a, 'b, 'c, 'a, 'd, 'e)) { 
		compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	}
	
	// P9 - pack
	expect(List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))) {
		pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	}
	
	// P10 - encode
	expect(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))) {
		encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) 
	}
	
	// P11 - encodeModified
	expect(List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))) {
		encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	}
	
	// P12 - decode
	expect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) {
		decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
	}
	
	// P13 - encodeDirect
	expect(List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))) {
		encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
	}
	
	// P14 - duplicate
	expect(List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)) {
		duplicate(List('a, 'b, 'c, 'c, 'd))
	}
	
	// P15 - duplicateN
	expect(List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)) {
		duplicateN(3, List('a, 'b, 'c, 'c, 'd))
	}
	
	// P16 - drop
	expect(List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)) {
		drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	}

	// P17 - split
	expect((List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))) {
		split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	}
	
	// P18 - slice
	expect(List('d, 'e, 'f, 'g)) {
		slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	}
	
	// P19 - rotate
	expect(List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)) {
		rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	}
	
	// P19 - rotate
	expect(List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)) {
		rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
	}
	
	// P20 - removeAt
	expect((List('a, 'c, 'd),'b)) {
		removeAt(1, List('a, 'b, 'c, 'd))
	}
	
	// P21 - insertAt
	expect(List('a, 'new, 'b, 'c, 'd)) {
		insertAt('new, 1, List('a, 'b, 'c, 'd))
	}
	
	// P22 - range
	expect(List(4, 5, 6, 7, 8, 9)) {
		range(4, 9)
	}
	
	// P23 - randomSelect	
	it("Should extract randomly selected elements") {
		val ls = List('a, 'b, 'c, 'd, 'f, 'g, 'h)
		val l = randomSelect(3, ls)
		assert(l.length == 3)
		assert(l(0) != l(1) && l(1) != l(2))
		l.foreach(x => assert(ls.indexOf(x) > -1))
	}
	
	// P24 - lotto
	it("Should draw 6 different random numbers from 1 to 49") {
		val l = lotto(6,  49) 
		assert(l.length == 6)
		l.foreach(x => assert(x >= 1 && x <= 49))
	}
	
}