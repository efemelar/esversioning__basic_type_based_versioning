package simpleCQRS

abstract class Event extends Message {
  var version: Int = -1

  def withVersion(v: Int) = {
    version = v
    this
  }
}

case class InventoryItemDeactivated_v1(id: UUID) extends Event
case class InventoryItemDeactivated_v2(itemId: UUID, reason: String) extends Event

case class InventoryItemCreated(id: UUID, name: String) extends Event

case class InventoryItemRenamed(id: UUID, newName: String) extends Event

case class ItemsCheckedInToInventory(id: UUID, count: Int) extends Event

case class ItemsRemovedFromInventory(id: UUID, count: Int) extends Event

