package ch2

object MonoidLaws extends App {


  //╭──────────────────────────────────────────────────────────────────────────────────╮
  //│ 1. Monoid Laws                                                                   │
  //│                                                                                  │
  //│ Left identity:      List() ::: List(1,2) == List(1,2)                            │
  //│ Right identity:     List(1,2) ::: List() == List(1,2)                            │
  //│ Associativity:      (List1 ::: List2) ::: List3 == List1 ::: (List2 ::: List3)   │
  //│                                                                                  │
  //╰──────────────────────────────────────────────────────────────────────────────────╯


  //╭─────────────────────╮
  //│ 2. Merging lists    │
  //╰─────────────────────╯
  private val l12 = List(1, 2)
  private val l34 = List(3, 4)
  private val l56 = List(5, 6)
  private val l78 = List(7, 8)

  val list1: List[Int] = l12 ::: l34 ::: l56 ::: l78
  //                   ((l12 ::: l34) ::: l56)::: l78
  val list2: List[Int] = l12 ::: (l34 ::: (l56 ::: l78))

  println(s"list1: $list1")
  println(s"list2: $list2")


  // Operation on lists in details
  //╭──────────────────────────────────────┬┬──────────────────────────────────────────╮
  //│ list1 -> l12 ::: l34 ::: l56 ::: l78 ││ list2 -> l12 ::: (l34 ::: (l56 ::: l78)) │
  //│ 1,                                   ││ 5,                                       │
  //│ 1, 2                                 ││ 5, 6                                     │
  //│ 1, 2, 3, 4 -> Add                    ││ 5, 6, 7, 8 - Add                         │
  //│ 1,                                   ││ 3,                                       │
  //│ 1, 2,                                ││ 3, 4                                     │
  //│ 1, 2, 3                              ││ 3, 4, 5, 6, 7, 8 -> Add                  │
  //│ 1, 2, 3, 4                           ││ 1,                                       │
  //│ 1, 2, 3, 4, 5, 6  -> Add             ││ 1, 2,                                    │
  //│ 1,                                   ││ 1, 2, 3, 4, 5, 6, 7, 8 -> Add            │
  //│ 1, 2,                                ││                                          │
  //│ 1, 2, 3                              ││                                          │
  //│ 1, 2, 3, 4                           ││                                          │
  //│ 1, 2, 3, 4, 5, 6                     ││                                          │
  //│ 1,                                   ││                                          │
  //│ 1, 2,                                ││                                          │
  //│ 1, 2, 3                              ││                                          │
  //│ 1, 2, 3, 4                           ││                                          │
  //│ 1, 2, 3, 4, 5                        ││                                          │
  //│ 1, 2, 3, 4, 5, 6                     ││                                          │
  //│ 1, 2, 3, 4, 5, 6, 7, 8 -> Add        ││                                          │
  //│                                      ││                                          │
  //│                                      ││                                          │
  //│      :::                             ││                                          │
  //│     ╱   ╲                            ││                                          │
  //│    ╱     ╲                           ││                                          │
  //│  1,2     :::                         ││                                          │
  //│         ╱   ╲                        ││                                          │
  //│        ╱     ╲                       ││                                          │
  //│      3,4     :::                     ││                                          │
  //│             ╱   ╲                    ││                                          │
  //│            ╱     ╲                   ││                                          │
  //│           5,6    7,8                 ││                                          │
  //│                                      ││                                          │
  //╰──────────────────────────────────────┴┴──────────────────────────────────────────╯


  //╭──────────────────────────╮
  //│ 3. Merging strings       │
  //╰──────────────────────────╯
  println("╭───────────╮\n│" + "Hello " ++ "John" ++ "." ++ "│\n╰───────────╯")
  println("╭───────────╮\n│" + ("Hello " ++ "John" ++ ".") ++ "│\n╰───────────╯")

  def between(prefix: String, msg: String, suffix: String): String = prefix + msg + suffix

  val textInBox = between(
    prefix = "╭───────────╮\n│",
    msg = "Hello " ++ "John" ++ ".",
    suffix = "│\n╰───────────╯")
  println(s"Function:\n$textInBox")

  //  Are we allowed to do this?
  // ╭─────────────────────────────────────────────────┬┬───────────────────────────────────────────╮
  // │                   +                             ││                    +                      │
  // │                  ╱ ╲                            ││                   ╱ ╲                     │
  // │                 ╱   ╲                           ││                  ╱   ╲                    │
  // │ ╭───────────╮\n│     +                          ││  ╭───────────╮\n│     +                   │
  // │                     ╱ ╲                         ││                      ╱ ╲                  │
  // │                    ╱   ╲                        ││                     ╱   ╲                 │
  // │               Hello     +                       ││                    +     +                │
  // │                        ╱ ╲                      ││                   ╱ ╲    │\n╰───────────╯ │
  // │                       ╱   ╲                     ││                  ╱   ╲                    │
  // │                   John     +                    ││               Hello   +                   │
  // │                           ╱ ╲                   ││                      ╱ ╲                  │
  // │                          ╱   ╲                  ││                     ╱   ╲                 │
  // │                         .     │\n╰───────────╯  ││                 John     .                │
  // ╰─────────────────────────────────────────────────┴┴───────────────────────────────────────────╯


  //╭───────────────────────╮
  //│ 4. Generalization     │
  //╰───────────────────────╯
  import cats._
  import cats.syntax.monoid._

  def betweenM[M](prefix: M, content: M, suffix: M)(implicit monoid: Monoid[M]): M =
    prefix |+| content |+| suffix

  import cats.instances.string._

  val textInBox2 = betweenM(
    prefix = "╭───────────╮\n│",
    content = "Hello " ++ "John" ++ ".",
    suffix = "│\n╰───────────╯")
  println(s"Monoid:\n$textInBox2")


  //╭───────────────────────╮
  //│ 5. Reuse              │
  //╰───────────────────────╯
  import cats.instances.int._

  println(s"betweenM(1,2,3): ${betweenM(1, 2, 3)}")


  import cats.instances.option._
  import cats.syntax.option._

  println(s"betweenM(1.some, 2.some, 3.some): ${betweenM(1.some, 2.some, 3.some)}")
  println(s"betweenM(1.some, None, 3.some): ${betweenM(1.some, none[Int], 3.some)}")
  //  List(1.some, none[Int], 3.some).flatMap(_.toList).sum
  //  List(1.some, none[Int], 3.some).map(_.getOrElse(0)).sum

  import cats.instances.list._

  println(s"betweenM(List(0), List(1, 2, 1), List(0)): ${betweenM(List(0), List(1, 2, 1), List(0))}")





  //╭─────────────────────────────────────────────────────────╮
  //│ 7. Based on https://www.youtube.com/watch?v=VzNGF4V937o │
  //╰─────────────────────────────────────────────────────────╯

}
