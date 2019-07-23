import AliveCell.toCell
import World.evolve
import GameOfLifeSpec.fourDiagonalNeighbours
import org.scalatest.FunSpec
import org.scalatest.words.ShouldVerb
import org.scalatest.Matchers._

trait Cell {
  val x: Int
  val y: Int

  def evolve(neighbourhood: Set[AliveCell]): Option[AliveCell]
}

case class AliveCell(x: Int, y: Int) extends Cell {
  override def evolve(neighbourhood: Set[AliveCell]): Option[AliveCell] = {
    val isSustainable = neighbourhood.size > 2
    val isNotOverPopulated = neighbourhood.size <= 4
    val shouldStayAlive = isSustainable && isNotOverPopulated
    if (shouldStayAlive) {
      Some(this)
    } else {
      None
    }
  }
}

case class DeadCell(x: Int, y: Int) extends Cell {
  override def evolve(neighbourhood: Set[AliveCell]): Option[AliveCell] = {
    if (neighbourhood.size == 3) {
      Some(AliveCell(x, y))
    } else {
      None
    }
  }
}

object AliveCell {
  def toCell(position: (Int, Int)): AliveCell = AliveCell(position._1, position._2)
}

object World {

  def evolve(world: Set[AliveCell]): Set[AliveCell] = {

    def potentialWorld = world.flatMap(cell => {
      val myNeighbours = for {
        x <- Set(-1, 0, 1)
        y <- Set(-1, 0, 1)
        if !world.contains(AliveCell(cell.x + x, cell.y + y))
      } yield DeadCell(cell.x + x, cell.y + y)
      myNeighbours ++ Set(cell)
    })

    def neighbour(cell: Cell): Set[AliveCell] = world.filter(other => Math.abs(cell.x - other.x) <= 1 && Math.abs(cell.y - other.y) <= 1)

    def evolveOneCell(cell: Cell) = cell.evolve(neighbour(cell))

    potentialWorld.flatMap(evolveOneCell)
  }
}


class GameOfLifeSpec extends FunSpec with ShouldVerb {

  val cellAtOrigin = AliveCell(0, 0)
  val upperCell = AliveCell(0, 1)
  val upperLeftCell = AliveCell(-1, 1)
  val bottomRightCell = AliveCell(1, -1)
  val upperRightCell = AliveCell(1, 1)
  val rightCell = AliveCell(1, 0)
  val lowerLeftCell = AliveCell(-1, -1)

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
      newWorld should contain(cellAtOrigin)
      //      newWorld should be(Set(cellAtOrigin))
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
      newWorld should not contain cellAtOrigin
    }
  }

  describe("Reproduction") {
    it("A dead cell with 3 alive neighbours should come to life") {
      val world = Set(upperRightCell, lowerLeftCell, upperLeftCell)
      val nextWorld = evolve(world)
      nextWorld should be(Set(cellAtOrigin))
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