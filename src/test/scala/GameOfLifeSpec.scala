import org.scalatest.FunSpec

class GameOfLifeSpec extends FunSpec {

  case class Cell(x: Int, y: Int)

  def evolve(emptyWorld: Set[Cell]) = Set.empty[Cell]

  describe("Empty world") {
    it("should remain empty") {
      val emptyWorld = Set.empty[Cell]
      val newWorld = evolve(emptyWorld)
      assert(emptyWorld.equals(newWorld))
    }
  }

}