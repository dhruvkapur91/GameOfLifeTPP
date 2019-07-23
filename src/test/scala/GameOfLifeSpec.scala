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

  describe("Underpopulated world") {
    it("Cell with no neighbours die of loneliness") {
      val lonelyCell = Cell(0,0)
      val world = Set(lonelyCell)
      val newWorld = evolve(world)
      assert(Set.empty[Cell].equals(newWorld))
    }
  }


}