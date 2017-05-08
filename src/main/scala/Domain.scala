package simpleCQRS



class InventoryItem (
  val id: UUID,
  events: Seq[Event],
  isNew: Boolean
) extends AggregateRoot[InventoryItem](
  events,
  isNew
) {
  type AState = State

  def this(id: UUID, name: String) =
    this(id, Seq(InventoryItemCreated(id, name)), true)

  case class State(
    id: UUID,
    name: String,
    activated: Boolean,
    count: Int)

  def apply = {
    case e: InventoryItemCreated =>
      State(id = e.id, name = e.name, activated = true, count = 0)

    case _: InventoryItemDeactivated_v1 =>
      state.copy(activated = false)

    case _: InventoryItemDeactivated_v2 =>
      state.copy(activated = false)

    case e: InventoryItemRenamed =>
      state.copy(name = e.newName)

    case e: ItemsCheckedInToInventory =>
      state.copy(count = state.count + e.count)

    case e: ItemsRemovedFromInventory =>
      state.copy(count = state.count - e.count)
  }

  def changeName(newName: String) {
    require(!newName.isEmpty, "New name must be defined")
    applyChange(InventoryItemRenamed(id, newName))
  }

  def remove(count: Int) {
    require(count > 0, "cant remove negative count from inventory")
    applyChange(ItemsRemovedFromInventory(id, count))
  }

  def checkIn(count: Int) {
    require(count > 0, "must have a count greater than 0 to add to inventory")
    applyChange(ItemsCheckedInToInventory(id, count))
  }

  def deactivate(reason: String) {
    require(state.activated, "already deactivated")
    applyChange(InventoryItemDeactivated_v2(id, reason))
  }
}

abstract class AggregateRoot[T <: AggregateRoot[T]] protected(es: Seq[Event], isNew: Boolean) { this: T =>

  type AState

  private var changes: List[Event] = Nil
  private var version: Int = 0
  private var aState: AState = _

  es.foldLeft(this)(_.applyChange(_, isNew))

  def this(events: Event*) = this(events, true)

  def id: UUID

  def apply: PartialFunction[Event, AState]

  protected def state = aState

  def uncommittedChanges = changes

  def markChangesAsCommitted() {
    changes = Nil
  }

  protected def applyChange(e: Event): T =
    applyChange(e, true)

  private def applyChange(e: Event, isNew: Boolean): T = {
    val ignore = (e: Event) => {
      println(Console.RED+s"Ignoring unknown event $e"+Console.RESET)
      state
    }
    aState = this.apply.applyOrElse(e, ignore)
    if (isNew) changes :+= e
    this
  }
}

trait Repository[T <: AggregateRoot[T]] {
  def save(aggregate: AggregateRoot[T], expectedVersion: Int = -1)
  def getById(id: UUID): T
}

import scala.reflect._

class EventStoreRepository[T <: AggregateRoot[T] : ClassTag](
  storage: EventStore
) extends Repository[T] {

  def save(ar: AggregateRoot[T], expectedVersion: Int = -1) {
    storage.saveEvents(ar.id, ar.uncommittedChanges, expectedVersion)
  }

  def getById(id: UUID) =
    classTag[T].runtimeClass.getConstructor(classOf[UUID], classOf[Seq[Event]], classOf[Boolean])
    .newInstance(id, storage.eventsForAggregate(id), java.lang.Boolean.FALSE)
    .asInstanceOf[T]
}

