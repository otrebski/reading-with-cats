package variance

object Grapics extends App {

  //Based on http://blog.originate.com/blog/2016/08/10/cheat-codes-for-contravariance-and-covariance/
  object Coviariance {
    class Entertainment
    class Music extends Entertainment
    class DeathMetal extends Music

    trait Producer[+X] {
      def produce(): X
    }

    class Clown extends Producer[Entertainment] {
      override def produce(): Entertainment = new Entertainment
    }

    class Musician extends Producer[Music] {
      override def produce(): Music = new Music
    }

    class DeathMetalSinger extends Producer[DeathMetal] {
      override def produce(): DeathMetal = new DeathMetal
    }
  }
  import Coviariance._

  val e: List[Producer[Entertainment]] = List(new Clown, new Musician, new DeathMetalSinger)
  val m: List[Producer[Music]] = List(new Musician, new DeathMetalSinger)
  val d: List[Producer[DeathMetal]] = List(new DeathMetalSinger)

  //======================================
  //======================================
  //======================================

  object Contravariance {

    class EnergySource(val calories: Int)

    class Vegetable(
                     override val calories: Int,
                     val species: String)
      extends EnergySource(calories)

    class Bamboo(
                  override val calories: Int,
                  override val species: String,
                  val softness: Int)
      extends Vegetable(calories, species)

    trait Consumer[-A] {
      def consume(a: A): Unit
    }

    class Burner extends Consumer[EnergySource] {
      override def consume(a: EnergySource): Unit =
        println(s"Burner consuming: ${a.getClass.getName} with ${a.calories} calories")
    }

    class Hipster extends Consumer[Vegetable] {
      override def consume(v: Vegetable): Unit =
        println(s"Hipster consuming: ${v.getClass.getName} ${v.species} with ${v.calories} calories")
    }

    class Panda extends Consumer[Bamboo] {
      override def consume(v: Bamboo): Unit =
        println(s"Panda consuming: ${v.getClass.getName} ${v.species} with ${v.calories} calories. Softness: ${v.softness}")
    }

  }

  import Contravariance._

  println()
  private val energySource = new EnergySource(calories = 2)
  private val vegetable = new Vegetable(calories = 100, species = "Carrot")
  private val bamboo = new Bamboo(calories = 120, species = "Arundinarieae", softness = 20)

  private val burner = new Burner
  private val hipster = new Hipster
  private val panda = new Panda

  private val all: List[Consumer[Bamboo]] = List(burner, hipster, panda)
  println("Consumers of bamboo:")
  all.foreach(_.consume(bamboo))
//  all.foreach(_.consume(vegetable)) -> Compilation error
//  panda.consume(vegetable) // --> Compilation error

  private val consumersOfVegetable: List[Consumer[Vegetable]] = List(burner, hipster)
  println("\nConsumers of vegetable:")
  consumersOfVegetable.foreach(c => c.consume(bamboo))
  consumersOfVegetable.foreach(c => c.consume(vegetable))
//  consumersOfVegetable.foreach(c => c.consume(energySource)) --> Compilation error

  println("\nConsumer fo energy source:")
  burner.consume(bamboo)
  burner.consume(vegetable)
  burner.consume(energySource)

  object EqExample {
    import cats.Eq
    import cats.syntax.eq._
    // ╭───────────────────┬┬────────────────────────────────────╮
    // │                   ││      Consumer[-A]                  │
    // │                   ││                                    │
    // │ EnergySource      ││ Panda   -> Consumer[Bamboo]        │
    // │     ╱╲            ││  ╱╲         ╱╲                     │
    // │     ││            ││  ││         ││                     │
    // │ Vegetable         ││ Hipster -> Consumer[Vegetable]     │
    // │     ╱╲            ││  ╱╲         ╱╲                     │
    // │     ││            ││  ││         ││                     │
    // │ Bamboo            ││ Burner  -> Consumer[EnergySource]  │
    // ╰───────────────────┴┴────────────────────────────────────╯

    implicit val bambooEq: Eq[Bamboo] = (x: Bamboo, y: Bamboo) =>
      x.species == y.species && x.calories == y.calories && x.softness == y.softness

    implicit val vegetableEq: Eq[Vegetable] = (x: Vegetable, y: Vegetable) =>
      x.species == y.species && x.calories == y.calories

    val bamboo1 = new Bamboo(100, "a", 1)
    val bamboo2 = new Bamboo(101, "b", 1)
    val vegetable1 = new Vegetable(101, "b")
    val vegetable2 = new Vegetable(101, "b")

    bamboo1 === bamboo2
    vegetable1 === vegetable2
    vegetable1 === bamboo1
//    bamboo1 === vegetable1
  }

}
