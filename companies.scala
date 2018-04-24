abstract class Entity(name: String) {
  import scala.collection.mutable.Map
  var interests: Map[Entity, Set[Int]] = Map()
  var total: Int = 0

  override def toString() = s"$name"
  def issue(buyer: Entity, amount: Int) = {
    if (interests.contains(buyer)) {
      interests(buyer) ++ (total until (total + amount)).toSet
      total += amount
    } else {
      interests(buyer) = (total until (total + amount)).toSet
      total += amount
    }
  }

  def transfer(seller: Entity, buyer: Entity, amount: Int): Unit = {
    if (!interests.contains(buyer) && interests.contains(seller) && interests(seller).size >= amount) {
      interests(buyer) = interests(seller).take(amount)
      interests(seller) = interests(seller).drop(amount)
    } else if (interests.contains(buyer) && interests.contains(seller) && interests(seller).size >= amount) {
      interests(buyer) = interests(buyer) ++ interests(seller).take(amount)
      interests(seller) = interests(seller).drop(amount)
    } else println(seller + " does not have a sufficient interest in " + name + " to complete the transfer.")
  }

  def ledger() = {
    for ((k, v) <- interests) {
      val n = (v.size.toDouble / total) * 100
      println(k + ": " + f"$n%1.2f" + "%")
    }
  }
  
}

case class Person(name: String) extends Entity(name) {
  override def transfer(seller: Entity, buyer: Entity, amount: Int) = println("Invalid")
  override def issue(buyer: Entity, amount: Int) = println("Invalid")
  override def ledger() = println("Invalid")
}

case class Corporation(name: String) extends Entity(name)

case class Partnership(name: String) extends Entity(name)
