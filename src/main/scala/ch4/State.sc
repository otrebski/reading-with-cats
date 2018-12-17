
import cats.data.State

val a = State[Int, String] { a => (a + 1, "message") }
a.run(10).value
a.runS(10).value
a.runA(10).value

val step1 = State[Int, String] { a => (a + 1, s"Message1 $a") }
val step2 = State[Int, String] { a => (a * 2, s"Message2 $a") }

val flow = for {
  s1 <- step1
  s2 <- step2
} yield s"$s1 | $s2"

flow.run(2).value
State.get[Int].run(10).value
State.get[String].run("A").value
State.set[Int](1).run(2).value

State.pure[Int, String]("Result").run(10).value 
State.inspect[Int, String](_ + "!").run(1).value
State.modify[Int](_ + 2).run(1).value           