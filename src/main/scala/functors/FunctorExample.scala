package functors

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.language.postfixOps
import scala.util.{Success, Try}

import cats.Functor
import cats.syntax.option._
import functors.FunctorExample.SuperGeneric.countChars
import util.Util._

object FunctorExample extends App {

  // ╭─────────────────────╮
  // │  Standard library   │
  // ╰─────────────────────╯
  println("Standard library".inBox())
  println(Some("abc").map(_.length))
  println(List("", "A").map(_.length))
  println(Try("").map(_.length))

  // ╭──────────────╮
  // │  Custom type │
  // ╰──────────────╯

  case class Box[A](value: A)
  val box = Box("aa")
  println(box.value.length)

  // ╭────────────────────────╮
  // │ Single mapping method  │
  // ╰────────────────────────╯

  object SingleMethod {
    def countChars[A](x: A): A = {
      (x match {
        case Some(v: String) => Some(v.length)
//      case a: List[String] => a.map(_.length)
//      case t: Try[String] => t.map(_.length)
        case Box(v: String) => Box(v * 2)
        case a: Any => a
      }).asInstanceOf[A]
    }
  }
  println("Single mapping method".inBox())
  println(SingleMethod.countChars(Some(1)))

  // ╭────────────────────────╮
  // │ Overloaded method      │
  // ╰────────────────────────╯
  object OverLoadedMethod {
    def countChars(s: Option[String]): Option[Int] = s.map(_.length)
    def countChars(s: List[String]): List[Int] = s.map(_.length)
    def countChars(s: Try[String]): Try[Int] = s.map(_.length)
    def countChars(s: Box[String]): Box[Int] = Box(s.value.length)
  }
  println("Overloaded method".inBox())
  println(OverLoadedMethod.countChars(Some("a")))
  println(OverLoadedMethod.countChars(List("av", "cv")))
  println(OverLoadedMethod.countChars(Success("Super")))

  // ╭──────────────╮
  // │ Converter    │
  // ╰──────────────╯

  object Converter {

    trait Counter[A, F[_]] {
      def count(a: F[A]): F[Int]
    }

    val optionCounter: Converter.Counter[String, Option] = (a: Option[String]) => a.map(_.length)

    class ListCounter extends Counter[String, List] {
      override def count(a: List[String]): List[Int] = a.map(_.length)
    }

    def countChars[F[_]](string: F[String])(counter: Counter[String, F]): F[Int] = {
      counter.count(string)
    }
  }

  println("Converter".inBox())
  println(Converter.countChars("abc".some)(Converter.optionCounter))
  println(Converter.countChars(List("a", "B", "C"))(new Converter.ListCounter()))
  println(Converter.countChars(Try("aC"))((a: Try[String]) => a.map(_.length)))

  // ╭──────────────────╮
  // │ Functors         │
  // ╰──────────────────╯

  object Functors {
    import cats.Functor
    import cats.instances.list._
    import cats.instances.option._
    import cats.syntax.functor._ // for Functor

    def test(): Unit = {
      val function: String => Int = _.length
      println(Functor[Option].map(Some("A"))(function))
      println(Functor[List].map(List("abc"))(function))
    }

    // ╭──────────────────────────────────────────────╮
    // │ Functor with predefined functionality        │
    // ╰──────────────────────────────────────────────╯
    def countChars[F[_]](start: F[String])(implicit functor: Functor[F]): F[Int] = {
      val r = start.map(x => x.length)
      r
    }
  }

  // ╭───────────────────────╮
  // │  Usage of 'Functor'   │
  // ╰───────────────────────╯
  println("Functor".inBox())
  import cats.instances.list._
  import cats.instances.option._
  import cats.instances.try_._

  println(Functors.countChars("aa".some))
  println(Functors.countChars(List("a", "bc", "def")))
  println(Functors.countChars(Try("fg")))

  // ╭─────────────────────╮
  // │  Generic functor    │
  // ╰─────────────────────╯

  println("\nGeneric functor")
  private val countCharsFunction: String => Int = _.length

  println(List("a", "bc", "def").map(countCharsFunction))
  println(Try("abc").map(countCharsFunction))
  println("some text".some.map(countCharsFunction))

  // ╭──────────────────────────╮
  // │ Functor for custom types │
  // ╰──────────────────────────╯
  println("Functor for custom types".inBox())

  //Functor for Box
  implicit val boxFunctor: Functor[Box] = new Functor[Box] {
    override def map[A, B](fa: Box[A])(f: A => B): Box[B] = Box(f(fa.value))
  }
  import cats.syntax.functor._ //For map method

  println(Box(3).map(_ * 3))


  //Custom Tree structure
  object Trees {
    sealed trait Tree[+A]

    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    final case class Leaf[A](value: A) extends Tree[A]

    implicit val functor: Functor[Tree] = new Functor[Tree] {
      def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
        fa match {
          case Leaf(value) => Leaf(f(value))
          case Branch(left: Tree[A], right: Tree[A]) => Branch(map(left)(f), map(right)(f))
        }
    }
  }

  import Trees._
  val tree: Tree[Int] =
    Branch(
      Branch(
        Leaf(1),
        Branch(
          Leaf(2), Leaf(3)
        )
      ),
      Leaf(4)
    )

  println(tree.map(_ * 3))


  // ╭─────────────────────╮
  // │  SUPER GENERIC      │
  // ╰─────────────────────╯
  object SuperGeneric {

    def countChars[F[_]](typedByString: F[String])(implicit functor: Functor[F]): F[Int] = {
      functor.map(typedByString)(_.length)
    }
  }

  println("Super generic!".inBox())
  //Standard types
  println(countChars("sdf".some))
  println(countChars(List("a", "b", "c")))
  println(countChars(Try("AA")))

  import cats.instances.future._
  implicit val ec: ExecutionContextExecutor = ExecutionContext.global
  println(Await.result(countChars(Future.successful("Success")), 1 second))


  //Custom types
  println(countChars(Box("AAAA!")))
  //Functor for Tree[_] Already defined above
  println(SuperGeneric.countChars(Branch(Leaf("A"), Leaf("AB")): Tree[String]))

  // ╭────────────╮
  // │  Laws      │
  // ╰────────────╯
  println("Laws".inBox())
  // ╭───────────────────────╮
  // │ Identity law          │
  // │  fa.map(a => a) == fa │
  // ╰───────────────────────╯

  println(s"Identity law : ${tree == tree.map(p => p)}")

  // ╭─────────────────────────────────────╮
  // │ Composition law                     │
  // │ fa.map(g(f(_))) == fa.map(f).map(g) │
  // ╰─────────────────────────────────────╯

  case class Person(firstName: String, lastName: String, title: String)

  val treeNames: Tree[Person] = Branch(
    Leaf(Person("Jan", "Kowalski", "Mr")),
    Leaf(Person("Jan", "Nowak", "phd"))
  )

  val format: Person => String = p => s"${p.title} ${p.firstName} ${p.lastName}"
  val count: String => Int = string => string.length

  private val mappings: Tree[Int] = treeNames.map(format).map(count)
  println(mappings)

  private val composition: Tree[Int] = treeNames.map(p => count(format(p)))
  println(composition)

  println(s"Composition law: ${composition == mappings}")

}
