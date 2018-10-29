package variance

object Animals extends App {
  abstract class Animal {
    def name: String
  }
  case class Cat(name: String) extends Animal
  case class Dog(name: String) extends Animal

  def printAnimalNames(animals: List[Animal]): Unit = {
    animals.foreach(animal => print(s"${animal.name}, "))
    println()
  }

  private val cats = List(Cat("c1"), Cat("c2"))
  private val dogs = List(Dog("d1"), Dog("d2"))
  printAnimalNames(cats)
  printAnimalNames(dogs)
  printAnimalNames(cats ::: dogs)

  abstract class Printer[-A] {
    def print(value: A): Unit
  }

  class AnimalPrinter extends Printer[Animal] {
    override def print(value: Animal): Unit = println(s"Animal: ${value.name}")
  }

  class DogPrinter extends Printer[Dog] {
    override def print(value: Dog): Unit = println(s"Dog: ${value.name}")
  }

  class CatPrinter extends Printer[Cat] {
    override def print(value: Cat): Unit = println(s"Cat: ${value.name}")
  }

  println("\nCats")
  new AnimalPrinter().print(Cat("cat"))
  new CatPrinter().print(Cat("cat"))
//  new DogPrinter().print(Cat("cat")) --> compile error

  println("\nMixed")
  (cats ::: dogs).foreach(new AnimalPrinter().print)
//  (cats ::: dogs).foreach(new DogPrinter().print) --> compile error


  class Cage[A](value: A) {
    private var _value: A = value
    def getValue: A = _value
    def setValue(value: A): Unit = {
      _value = value
    }
  }

  private val dogCage = new Cage[Dog](Dog("Dog"))
  println(dogCage.getValue)


}
