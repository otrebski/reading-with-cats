package ch2

import cats._
import cats.instances.map._
import cats.syntax.semigroup._

object Containers2 extends App {


  object Domain {

  case class Container(id: String, inventory: Inventory, subContainers: List[Container] = List.empty[Container])

  case class Inventory(content: Map[String, Quantity])

  trait Quantity

  case class Each(count: Int) extends Quantity

  case class Weight(grams: Int) extends Quantity
  }

  object DomainSyntax {
    import Domain._
    implicit val eachSemigroup = new Semigroup[Each] {

      override def combine(x: Each, y: Each): Each = Each(x.count + y.count)
    }

    implicit val weighSemigroup = new Semigroup[Weight] {

      override def combine(x: Weight, y: Weight): Weight = Weight(x.grams + y.grams)
    }

    implicit val quantitySemigroup = new Semigroup[Quantity] {

      override def combine(x: Quantity, y: Quantity): Quantity = (x, y) match {
        case (w1: Weight, w2: Weight) => w1 |+| w2
        case (w1: Each, w2: Each) => w1 |+| w2
        case _ => sys.error("I can't mix weight and eaches")
      }
    }


    implicit val inventorySemigroup = new Semigroup[Inventory] {

      override def combine(x: Inventory, y: Inventory): Inventory = {
        val content1: Map[String, Quantity] = x.content
        val content2: Map[String, Quantity] = y.content
        Inventory(content1 |+| content2)
      }
    }

  }

  import DomainSyntax._
  import Domain._

  def allInventory(container: Container, inventory: Inventory = Inventory(Map.empty)): Inventory = {
    container.subContainers match {
      case Nil =>
        inventory |+| container.inventory
      case list: List[Container] =>
        val inventories: List[Inventory] = list
          .flatMap(c => c.inventory :: c.subContainers.map(i => allInventory(i, Inventory(Map.empty))))
        val i: Inventory = inventories
          .foldLeft(Inventory(Map.empty))(_ |+| _)
        inventory |+| container.inventory |+| i
    }
  }

  val i1 = Inventory(Map(
    "Cola" -> Weight(1200),
    "Pepsi" -> Weight(750),
    "Wodka" -> Weight(100)
  ))


  val i2 = Inventory(Map(
    "Cola" -> Weight(250),
    "Pepsi" -> Weight(1500),
    "Wodka" -> Weight(250)
  ))

  val i3 = Inventory(Map(
    "Wodka" -> Weight(250),
    "Snickers" -> Each(20)
  ))

  private val container = Container("l", i1,
    List(
      Container("l2", i2, List.empty),
      Container("l3", i3, List.empty))
  )

  println("Container")
  println(container)
  println("All inventory")
  println(allInventory(container))
}
