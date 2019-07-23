import Cell.toCell
import World.evolve
import GameOfLifeSpec.fourDiagonalNeighbours
import org.scalatest.FunSpec
import org.scalatest.words.ShouldVerb
import org.scalatest.Matchers._

case class Cell(x: Int, y: Int)

object Cell {
  def toCell(position: (Int, Int)): Cell = Cell(position._1, position._2)
}

object World {
  def evolve(world: Set[Cell]): Set[Cell] = {

    def neighbour(cell: Cell): Set[Cell] = world

    def evolveOneCell(cell: Cell) = {
      val myWorld = neighbour(cell)
      val isSustainable = myWorld.size > 2
      val isNotOverPopulated = myWorld.size <= 4
      val shouldStayAlive = isSustainable && isNotOverPopulated
      if (shouldStayAlive) {
        Some(Cell(0, 0))
      } else {
        None
      }
    }

    world.flatMap(evolveOneCell)
  }
}


class GameOfLifeSpec extends FunSpec with ShouldVerb {


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

  describe("Sustainable development :) ") {
    it("A cell with 2 neighbours keeps living") {
      val centerCell = Cell(0, 0)
      val upperLeftCell = Cell(-1, 1)
      val bottomRightCell = Cell(1, -1)
      val world = Set(centerCell, upperLeftCell, bottomRightCell)
      val newWorld = evolve(world)
      newWorld should be(Set(Cell(0, 0)))
    }
    it("A cell with 3 neighbours keeps living") {
      val centerCell = Cell(0, 0)
      val upperLeftCell = Cell(-1, 1)
      val bottomRightCell = Cell(1, -1)
      val upperRightCell = Cell(1, 1)
      val world = Set(centerCell, upperLeftCell, bottomRightCell, upperRightCell)
      val newWorld = evolve(world)
      newWorld should be(Set(Cell(0, 0)))
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

  def crossProduct[P, Q](first: Set[P], second: Set[Q]): Set[(P, Q)] = {
    for {
      a <- first
      b <- second
    } yield (a, b)
  }

  def selfProduct[T](set: Set[T]): Set[(T, T)] = {
    crossProduct(set, set)
  }

  def fourDiagonalNeighbours: Set[Cell] = {
    val diagonalDeltas = Set(-1, 1)
    selfProduct(diagonalDeltas).map(toCell)
  }
}