package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    value <- arbitrary[A]
    heap <- oneOf[H](empty, genHeap)
  } yield insert(value, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def heap2List(m: H): List[A] =
    if (isEmpty(m)) Nil
    else findMin(m) :: heap2List(deleteMin(m))

  def list2Heap(seq: Seq[Int]): H =
    if (seq.isEmpty) empty
    else insert(seq.head, list2Heap(seq.tail))

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

    property("spec1") = forAll { (x1: A, x2: A) =>
      val m = insert(x2, insert(x1, empty))
      findMin(m) == math.min(x1, x2)
    }

      property("spec2") = forAll { (x1: A) =>
        val m = deleteMin(insert(x1, empty))
        m == empty
      }

      property("spec3") = forAll { (m: H) =>

        val result = heap2List(m)
        result == result.sorted
      }

      property("spec4") = forAll { (m1: H, m2: H) =>
        val min = findMin(meld(m1, m2))
        min == findMin(m1) || min == findMin(m2)
      }

      property("insert list") = forAll { (seq: Seq[Int]) =>
        heap2List(list2Heap(seq)) == seq.sorted
      }
}
