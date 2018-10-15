package ch2

import cats.instances.list._
import cats.kernel.Semigroup
import cats.syntax.semigroup._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}
import cats.syntax.option._

object MonoidDemo extends App {


  object Lists {

    private val list1: List[Int] = List(0, 1)
    private val list2: List[Int] = List(3, 4)

    def test(): Unit = {
      println("Lists:")
      println(s"List(0, 1) |+| List(3, 4) ${list1 |+| list2}")
      println(s"List(0, 1) ::: List(3, 4) ${list1 ::: list2}")
    }

  }


  Lists.test()

  object Options {

    val option1: Option[Int] = none[Int]
    val option2: Option[Int] = none[Int]

    implicit val optionsSemigroup: Semigroup[Option[Int]] = new Semigroup[Option[Int]] {
      override def combine(x: Option[Int], y: Option[Int]): Option[Int] = (x, y) match {
        case (None, None) => None
        case (o1, o2) => Some(o1.getOrElse(0) + o2.getOrElse(0))
      }
    }

    def test(): Unit = {
      println("\n\nOptions:")

      println(s"option1.getOrElse(0) + option2.getOrElse(0): " +
        s"${option1.getOrElse(0) + option2.getOrElse(0)}")

      val sum = for {
        o1 <- option1
        o2 <- option2
      } yield o1 + o2
      println(s"for{...}: $sum")

      println(s"option1 |+| option2 ${option1 |+| option2}")
    }
  }

  Options.test()


  object Futures {
    implicit val ec: ExecutionContextExecutor = ExecutionContext.global

    private def loadFrom1(): Future[List[Int]] = Future.successful(List(0, 1))

    private def loadFrom2(): Future[List[Int]] = Future.successful(List(2, 3))

    def test(): Unit = {
      println("\n\nFutures:")

      val r1 = for {
        f1 <- loadFrom1()
        f2 <- loadFrom2()
      } yield f1 |+| f2
      println(s"Future |+| Future ${Await.result(r1, 1 second)}")

      val r2 = for {
        f1 <- loadFrom1()
        f2 <- loadFrom2()
      } yield f1 ::: f2

      println(s"Future ::: Future ${Await.result(r2, 1 second)}")

      import cats.instances.future._
      import cats.instances.int._
      val r: Future[Int] = Future.successful(1) |+| Future.successful(2)
      println(s"Future.successful(1) |+| Future.successful(2)=${Await.result(r, 1 second)}")

      print("Future.successful(1) |+| Future.failure(ex)=")
      val eventualInt = Future.successful(1) |+| Future.failed(new Exception("Ups!"))
      eventualInt.onComplete {
        case Success(value) => println(value)
        case Failure(ex) => println(ex.getMessage)
      }
      Await.ready(eventualInt, 1 second)
    }
  }

  Futures.test()

  object Tries {


    //
    //    implicit def trySemigroup = new Semigroup[Try[_]] {
    //
    //      override def combine[A](x: Try[A], y: Try[A]): Try[A] = (x, y) match {
    //        case (Success(s1), Success(s2)) => Success(s1 |+| s2)
    //        case (Failure(ex), _) => Failure(ex)
    //        case (_, Failure(ex)) => Failure(ex)
    //      }
    //
    //    }

    def test(): Unit = {
      println("\n\nTries:")
      import cats.instances.int._
      import cats.instances.try_._

      val try1: Try[Int] = Success(10)
      val try2: Try[Int] = Success(11)
      val tryError1: Try[Int] = Failure(new RuntimeException("E1"))
      val tryError2: Try[Int] = Failure(new RuntimeException("E2"))
      val tryError3: Try[Int] = Failure(new RuntimeException("E3"))

      println(try1 |+| try2)
      println(try1 |+| tryError1)
      println(tryError1 |+| tryError2)
      println(tryError1 |+| tryError2|+| tryError3)
      println(tryError1 |+| (tryError2|+| tryError3))
      println(tryError1 |+| try1)
    }
  }
  Tries.test()

}
