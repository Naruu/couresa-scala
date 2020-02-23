package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for{
     k <- arbitrary[A]
     h <- genHeap
    } yield insert(k,h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // empty heap => insert => min
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // merge => get min of 
  property("merge1") = forAll { (h: H, g:H) => {
    if(isEmpty(h)){
      if(isEmpty(g)) (meld(h,g) == empty) else (findMin(meld(h,g)) == findMin(g))
    }
    else {
      if(isEmpty(g)) (findMin(meld(h,g)) == findMin(h)) else (findMin(meld(h,g)) == min(findMin(g), findMin(h)))
      }
    }
  }

  property("del1") = forAll { (a: A) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
}

  property("sort1") = forAll { (h: H) => 
    def popMin(h:H): List[A] = if(isEmpty(h)) List.empty else findMin(h) :: popMin(deleteMin(h))
    val popped = popMin(h)
    popped == popped.sorted
  }

  property("add2") = forAll { (a: A, b: A) =>
    val hh = findMin(insert(b,insert(a, empty)))
    (hh == min(a,b))
  }

  //https://gist.github.com/wh5a/7394082
  property("meld") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
    if (isEmpty(h1) && isEmpty(h2)) true
    else {
      val m1 = findMin(h1)
      val m2 = findMin(h2)
            (m1 == m2) && heapEqual(deleteMin(h1), deleteMin(h2))
          }
      (!isEmpty(h1)) ==> heapEqual(meld(h1, h2),
                meld(deleteMin(h1), insert(findMin(h1), h2)))
      }
}
