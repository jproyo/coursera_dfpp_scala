package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  def toList(h: H): List[Int] = {
    if (isEmpty(h)) List[Int]()
    else findMin(h) :: toList(deleteMin(h))
  }

  def orderPreserved(h: H): Boolean= {
    if (isEmpty(h)) true
    else {
      val newElem = findMin(h)
      val newHeap = deleteMin(h)
      (isEmpty(newHeap)) || ((newElem <= findMin(newHeap)) && orderPreserved(newHeap))
    }
  }

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(genHeap, const(empty))
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min(empty) == 0 or min") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("find(min) == insert(min, empty)") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("deleteMin(h) | h(x) == empty") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("deleteMin(h) | h(x) == empty") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }


  property("findMin(h) == min(h.toList)") = forAll { (h: H) =>
    if (!isEmpty(h)) findMin(h) == toList(h).min
    else true
  }

  property("findMin(meld(h1, h2)) == min(h1) | min(h2)") = forAll { (h1: H, h2: H) =>
    if (!isEmpty(h1) && !isEmpty(h2)) {
      val expected = findMin(meld(h1, h2))
      findMin(h1) == expected || findMin(h2) == expected
    } else true
  }

  property("findMin(h1) == findMin(meld(h1, empty)) ") = forAll { (h1: H) =>
    if (!isEmpty(h1)) {
      findMin(h1) == findMin(meld(h1, empty))
    } else true
  }

  property("findMin(h1) == findMin(meld(empty, h1)) ") = forAll { (h1: H) =>
    if (!isEmpty(h1)) {
      findMin(h1) == findMin(meld(empty, h1))
    } else true
  }

  property("meld(h1, h2) -> remove elem h1 insert into h2 meld(h1, h2) preserve order") = forAll { (h1: H, h2: H) =>
    if(!isEmpty(h1)) {
      val min = findMin(meld(h1, h2))
      val minH1 = findMin(h1)
      deleteMin(h1)
      findMin(meld(insert(minH1, h2), h1)) == min
    } else true
  }

  property("delete and melt preserver min") = forAll { (h1: H, h2: H) =>
    if(!isEmpty(h2) && !isEmpty(h1)) {
      val newH1 = deleteMin(h1)
      val newH2 = deleteMin(h2)
      if (!isEmpty(newH1) && !isEmpty(newH2)){
        val min = findMin(newH1) min findMin(newH2)
        findMin(meld(newH1, newH2)) == min
      } else true
    } else true
  }

  property("delete and melt preserver order") = forAll { (h1: H, h2: H) =>
    if(!isEmpty(h2) && !isEmpty(h1)) {
      val newH1 = deleteMin(h1)
      val newH2 = deleteMin(h2)
      orderPreserved(meld(newH1, newH2))
    } else true
  }

  property("commutative meld") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == findMin(meld(h2, h1))
  }

  property("meld(h1, meld(h2, h3)) == meld(meld(h1, h2), h3)") = forAll { (h1: H, h2: H, h3: H) =>
    orderPreserved(meld(h1, meld(h2, h3))) && orderPreserved(meld(meld(h1, h2), h3))
  }

  property("findMin(h) == insert(findMin(h), deleteMin(h))") = forAll { (h: H) =>
    if (!isEmpty(h)) {
      findMin(insert(findMin(h), deleteMin(h))) == findMin(h)
    } else true
  }

  property("insert(y, insert(x, h1)) == insert(x, insert(y, h1))") = forAll { (h: H, x: Int, y: Int) =>
    findMin(insert(y, insert(x, h))) == findMin(insert(x, insert(y, h)))
  }

  property("preserve order after deletion") = forAll { (h: H) =>
    if (!isEmpty(h)) {
      orderPreserved(deleteMin(h))
    } else true
  }

  property("elements ordered. e1 <= e2 <= e3 ... <= en") = forAll { (h: H) =>
    orderPreserved(h)
  }

  property("preserve insertion order") = forAll { (l: List[Int]) =>
    orderPreserved((l foldRight empty)(insert))
  }

  property("preserve insertion order with mixed insertion") = forAll { (a: Int) =>
    val b = if (a == Int.MaxValue) a+1 else a
    deleteMin(insert(b+1, insert(b, insert(b+2, empty)))) == insert(b+2, insert(b+1, empty))
  }

  property("preserve insertion order") = forAll { (a: Int, b: Int) =>
    (a min b) == findMin(insert(b, insert(a, empty)))
  }
}
