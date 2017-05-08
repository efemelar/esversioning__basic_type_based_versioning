package simpleCQRS

import org.scalatest._


class InventoryItemDeactivationSpec extends FunSpec with GivenWhenThen {

  describe("Inventory Item") {
    it("can be deactivated at any time") {
      val app = new Boot
      import app._

      val itemId = randomUUID
      val item = new InventoryItem(
        itemId,
        Seq(InventoryItemCreated(itemId, "Test item")),
        isNew = true)

      Given("just created inventory item")
      repo.save(item)

      When("deactivating")
      bus.send(DeactivateInventoryItem(itemId, "too mainstream", 0))


      Then("deactivation reason noted down")
      assert(
        storage.eventsForAggregate(itemId).tail ===
        Seq(
          InventoryItemDeactivated_v2(itemId, "too mainstream")
        )
      )

      And("no details can be found about it afterwards")
      assert(
        readModel.findInventoryItemDetails(itemId).isEmpty
      )
    }
  }
}
