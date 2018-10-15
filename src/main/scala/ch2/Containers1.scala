package ch2

import cats._
import cats.syntax.semigroup._
import cats.instances.map._

object Containers1 extends App {


  object Domain {

    case class Container(id: String, inventory: Inventory, subContainers: List[Container] = List.empty[Container])

    case class Inventory(content: Map[String, Weight])

    case class Weight(grams: Int)

  }

  object DomainSyntax {

    import Domain._

    implicit val weighMonoid = new Monoid[Weight] {
      override def empty: Weight = Weight(0)

      override def combine(x: Weight, y: Weight): Weight = Weight(x.grams + y.grams)
    }

    implicit val inventoryMonoid = new Monoid[Inventory] {
      override def empty: Inventory = Inventory(Map.empty)

      override def combine(x: Inventory, y: Inventory): Inventory = {
        val content1: Map[String, Weight] = x.content
        val content2: Map[String, Weight] = y.content
        Inventory(content1 |+| content2)
      }
    }

  }

  import Domain._
  import DomainSyntax._

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
    "Wodka" -> Weight(250)
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
