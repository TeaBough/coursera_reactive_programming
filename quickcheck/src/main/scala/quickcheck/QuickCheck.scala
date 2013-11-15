package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll {
    a: Int =>
      val h = insert(a, empty)
      findMin(h) == a
  }

  property("gen1") = forAll {
    (h: H) =>
      val m = if (isEmpty(h)) 0 else findMin(h)
      findMin(insert(m, h)) == m
  }

  property("meld1") = forAll {
    (h1: H, h2: H) =>
      val m = if (isEmpty(h1)) 0 else findMin(h1)
      val m2 = if (isEmpty(h2)) 0 else findMin(h2)
      val mel = meld(h1, h2)
      findMin(mel) == Math.min(m, m2)
  }

  property("del1") = forAll {
    (h: H) =>
      if (!isEmpty(h)) {
        val m = findMin(h)
        findMin(deleteMin(insert(m, h))) == m
      }
      else true
  }

  property("2add") = forAll {
    (h: H) =>
      val h = insert(3, insert(1, empty))
      findMin(h) == 1
  }

  property("addEmpty") = forAll {
    (h: H) =>
      val hh = deleteMin(insert(3, empty))
      isEmpty(hh)
  }

  property("rec2") = forAll {
    (h: H) =>
      def rec(hh: H, el: List[Int]): List[Int] = {
        if (isEmpty(hh))
          el
        else {
          val m = findMin(hh)
          rec(deleteMin(hh), el.::(m))
        }
      }
      val ans = rec(h, List())
      ans.sorted == ans.reverse
  }

  property("meld2") = forAll {
    (h1: H, h2: H) =>
      def rec(hh: H, l: List[Int]): List[Int] = {
        if (isEmpty(hh))
          l
        else {
          rec(deleteMin(hh), l.::(findMin(hh)))
        }
      }
      val m1 = if (isEmpty(h1)) 0 else findMin(h1)
      val mel = meld(h1, h2)
      val mel2 = meld(deleteMin(h1), insert(m1, h2))
      rec(mel2, List()) == rec(mel, List())
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    h <- oneOf(empty, genHeap, genHeap, genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
