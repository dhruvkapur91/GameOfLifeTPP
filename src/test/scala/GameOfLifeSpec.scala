import AliveCell.toCell
import World.evolve
import GameOfLifeSpec.fourDiagonalNeighbours
import org.scalatest.FunSpec
import org.scalatest.words.ShouldVerb
import org.scalatest.Matchers._

trait Cell {
  val x: Int
  val y: Int

  def toAlive: AliveCell = AliveCell(x, y)
}

case class AliveCell(x: Int, y: Int) extends Cell

object AliveCell {
  def toCell(position: (Int, Int)): AliveCell = AliveCell(position._1, position._2)
}

object World {
  def evolve(world: Set[AliveCell]): Set[AliveCell] = {

    def neighbour(cell: Cell): Set[AliveCell] = world.filter(other => Math.abs(cell.x - other.x) <= 1 && Math.abs(cell.y - other.y) <= 1)

    def evolveOneCell(cell: Cell) = {
      val myWorld = neighbour(cell)
      val isSustainable = myWorld.size > 2
      val isNotOverPopulated = myWorld.size <= 4
      val shouldStayAlive = isSustainable && isNotOverPopulated
      if (shouldStayAlive) {
        Some(cell.toAlive)
      } else {
        None
      }
    }

    world.flatMap(evolveOneCell)
  }
}


class GameOfLifeSpec extends FunSpec with ShouldVerb {

  val cellAtOrigin = AliveCell(0, 0)
  val upperCell = AliveCell(0, 1)
  val upperLeftCell = AliveCell(-1, 1)
  val bottomRightCell = AliveCell(1, -1)
  val upperRightCell = AliveCell(1, 1)
  val rightCell = AliveCell(1, 0)

  val emptyWorld = Set.empty[AliveCell]


  describe("Empty world") {
    it("should remain empty") {
      val newWorld = evolve(emptyWorld)
      assert(emptyWorld.equals(newWorld))
    }
  }

  describe("Underpopulated world") {
    it("Cell with no neighbours die of loneliness") {
      val lonelyCell = cellAtOrigin
      val world = Set(lonelyCell)
      val newWorld = evolve(world)
      assert(emptyWorld.equals(newWorld))
    }

    it("Cell with one neighbour also die of loneliness") {
      val world = Set(cellAtOrigin, upperCell)
      val newWorld = evolve(world)
      assert(emptyWorld.equals(newWorld))
    }
  }

  describe("Sustainable development :) ") {
    it("A cell with 2 neighbours keeps living") {
      val centerCell = cellAtOrigin
      val world = Set(centerCell, upperLeftCell, bottomRightCell)
      val newWorld = evolve(world)
      newWorld should be(Set(cellAtOrigin))
    }
    it("A cell with 3 neighbours keeps living") {
      val centerCell = cellAtOrigin
      val world = Set(centerCell, upperLeftCell, bottomRightCell, upperRightCell)
      val newWorld = evolve(world)
      newWorld should be(Set(cellAtOrigin))
    }
    it("A world in where each cell has 3 neighbours does not change") {
      val world = Set(cellAtOrigin, rightCell, upperRightCell, upperCell)
      val nextWorld = evolve(world)
      nextWorld should be(world)
    }
  }

  describe("Overpopulated world") {
    it("Cell with 4 neighbours dies") {
      val world = Set(cellAtOrigin) ++ fourDiagonalNeighbours
      val newWorld = evolve(world)
      newWorld should be(Set.empty)
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

  def fourDiagonalNeighbours: Set[AliveCell] = {
    val diagonalDeltas = Set(-1, 1)
    selfProduct(diagonalDeltas).map(toCell)
  }
}