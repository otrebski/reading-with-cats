package ch2

import cats.{Semigroup, Show}
import cats.syntax.all._

object SemigroupLaws extends App {

  println("Hello")

  //╭──────────────────────────────────────────────────────────────────────────────────╮
  //│ 1. Semigroup Laws                                                                │
  //│                                                                                  │
  //│ Associativity:      (List1 ::: List2) ::: List3 == List1 ::: (List2 ::: List3)   │
  //│                                                                                  │
  //╰──────────────────────────────────────────────────────────────────────────────────╯






  //╭─────────────────────╮
  //│ 2. NonEmptyList     │
  //╰─────────────────────╯

  // There is no empty/neutral element
  final case class NonEmptyList[+A](head: A, tail: List[A] = Nil)


  implicit val nonEmptyListShow = new Show[NonEmptyList[String]] {
    override def show(t: NonEmptyList[String]): String = {
      s"""${t.head}${t.tail.mkString(", ", ", ", "")}"""
    }
  }

  implicit val nenEmptyListSemigroup = new Semigroup[NonEmptyList[String]] {
    override def combine(x: NonEmptyList[String], y: NonEmptyList[String]): NonEmptyList[String] = {
      val tail: List[String] = x.tail ::: List(y.head) ::: y.tail
      val r: NonEmptyList[String] = NonEmptyList(x.head, tail)
      r
    }
  }


  //╭─────────────────────╮
  //│ 3. Merging lists    │
  //╰─────────────────────╯

  private val listA = NonEmptyList("Apples", List("Apricots", "Avocados"))
  private val listB = NonEmptyList("Bananas", List("Blueberries"))
  private val listC = NonEmptyList("Cherries", List("Cucumbers"))

  private val listAB: NonEmptyList[String] = listA |+| listB
  private val listBC: NonEmptyList[String] = listB |+| listC

  private val listABC: NonEmptyList[String] = listA |+| listB |+| listC

  private val listAB_C: NonEmptyList[String] = listAB |+| listC
  private val listA_BC: NonEmptyList[String] = listA |+| listBC


  println(s"A       : ${listA.show}")
  println(s"B       : ${listB.show}")
  println(s"C       : ${listC.show}")
  println(s"A+B     : ${listAB.show}")
  println(s"B+C     : ${listBC.show}")
  println(s"A+B+C   : ${listABC.show}")
  println(s"(A+B)+C : ${listAB_C.show}")
  println(s"A +(B+C): ${listA_BC.show}")


}
