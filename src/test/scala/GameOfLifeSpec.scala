import org.scalatest.FunSpec

class GameOfLifeSpec extends FunSpec {

  def evolve(emptyWorld: Set[Nothing]) = Set.empty

  describe("Empty world") {
    it("should remain empty") {
      val emptyWorld = Set.empty
      val newWorld = evolve(emptyWorld)
      assert(emptyWorld.equals(newWorld))
    }
  }

}