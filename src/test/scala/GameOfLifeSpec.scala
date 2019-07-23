import World.evolve
import GameOfLifeSpec.fourDiagonalNeighbours
import org.scalatest.FunSpec

case class Cell(x: Int, y: Int)

object World {
  def evolve(emptyWorld: Set[Cell]) = Set.empty[Cell]
}


class GameOfLifeSpec extends FunSpec {


  describe("Empty world") {
    it("should remain empty") {
      val emptyWorld = Set.empty[Cell]
      val newWorld = evolve(emptyWorld)
      assert(emptyWorld.equals(newWorld))
    }
  }

  describe("Underpopulated world") {
    it("Cell with no neighbours die of loneliness") {
      val lonelyCell = Cell(0, 0)
      val world = Set(lonelyCell)
      val newWorld = evolve(world)
      assert(Set.empty[Cell].equals(newWorld))
    }

    it("Cell with one neighbour also die of loneliness") {
      val cellOne = Cell(0, 0)
      val cellTwo = Cell(0, 1)
      val world = Set(cellOne, cellTwo)
      val newWorld = evolve(world)
      assert(Set.empty[Cell].equals(newWorld))
    }
  }

  describe("Overpopulated world") {
    it("Cell with 4 neighbours dies") {
      val cellAtCenter = Cell(0, 0)
      val world = Set(cellAtCenter) ++ fourDiagonalNeighbours
      val newWorld = evolve(world)
      assert(newWorld.equals(Set.empty))
    }
  }


}

object GameOfLifeSpec {
  def fourDiagonalNeighbours: Set[Cell] = {
    val topLeftNeighbour = Cell(-1, 1)
    val topRightNeighbour = Cell(1, 1)
    val bottomRightNeighbour = Cell(1, -1)
    val bottomLeftNeighbour = Cell(-1, -1)
    Set(topLeftNeighbour, topRightNeighbour, bottomLeftNeighbour, bottomRightNeighbour)
  }
}