package ch4

import cats.data.State

object MyState extends App {

  val a = State[Int, String] { a => (a + 1, "message") }
  println(a.run(10).value) // (11,message)
  println(a.runS(10).value)       // 11
  println(a.runA(10).value)       // message

  val step1 = State[Int, String] { a => (a + 1, s"Message1 $a") }
  val step2 = State[Int, String] { a => (a * 2, s"Message2 $a") }

  val flow = for {
    s1 <- step1
    s2 <- step2
  } yield s"$s1 | $s2"

  println(flow.run(2).value)                 //(6,Message1 2 | Message2 3)
  println(State.get[Int].run(10).value)      //(10,10)
  println(State.get[String].run("A").value)  //(A,A)
  println(State.set[Int](1).run(2).value)    //(1,())

  println(State.pure[Int, String]("Result").run(10).value)  //(10,Result)
  println(State.inspect[Int, String](_ + "!").run(1).value) //(1,1!)
  println(State.modify[Int](_ + 2).run(1).value)            //(3,())
}
