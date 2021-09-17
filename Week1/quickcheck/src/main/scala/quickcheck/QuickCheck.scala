package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:

  lazy val genHeap: Gen[H] =
    for
      l <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    yield insert(l, h)

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (h: H) =>
    def isSorted(lastMin: Int, h: H): Boolean =
      if isEmpty(h) then true
      else
        val newMin = findMin(h)
        lastMin <= newMin && isSorted(newMin, deleteMin(h))
    isEmpty(h) || isSorted(findMin(h), deleteMin(h))
  }

  property("gen3") = forAll { (a: A) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("gen 4") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    findMin(meld(h1, h2)) == {
      if (min1 < min2) min1 else min2
    }
  }

  property("Two heaps should be equal if recursivly removing min elements result in same elements until empty") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

  property("gen5") = forAll { (a1: A, a2: A) =>
   val minval = if a1 < a2 then a1 else a2
   val maxval = if a1 > a2 then a1 else a2
   val heap = insert(a2, insert(a1, empty))
   findMin(heap) == minval && findMin(deleteMin(heap)) == maxval
  }