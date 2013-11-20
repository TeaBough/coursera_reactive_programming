package simulations

import math.random

class EpidemySimulator extends Simulator {
  //row - col
  type Position = (Int, Int)

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    val transmissionRate: Int = 40
    val prevalenceRate: Int = 1
    var firstInfected = population / 100 * prevalenceRate
    // to complete: additional parameters of simulation
  }

  import SimConfig._

  val persons: List[Person] = (1 to population).map({
    p =>
      val n: Person = new Person(p)
      if (p > 4) {
        n.infected = if (firstInfected > 0) {
          firstInfected = firstInfected - 1
          true
        } else false

        if (n.infected) n.incubation()
      }
      n
  }).toList

  // to complete: construct list of persons


  class Person(val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //

    def nextPosition(): Position = {
      randomBelow(4) + 1 match {
        case 1 => (row, (col + roomColumns - 1) % roomColumns)
        case 2 => ((row + 1) % roomRows, col)
        case 3 => (row, (col + 1) % roomColumns)
        case 4 => ((row + roomRows - 1) % roomRows, col)
      }
    }

    def isNextRoomNoticeablySafe(pos: Position): Boolean = {
      persons.filter(p => p.row == pos._1 && p.col == pos._2 && (p.sick || p.dead)).isEmpty
    }

    def isCurrentRoomReallySafe(): Boolean = {
      persons.filter(p => p.row == row && p.col == col && (p.infected || p.sick || p.dead)).isEmpty
    }

    def safeRooms(): List[Position] = {
      val pos1 = (row, (col + roomColumns - 1) % roomColumns)
      val pos2 = ((row + 1) % roomRows, col)
      val pos3 = (row, (col + 1) % roomColumns)
      val pos4 = ((row + roomRows - 1) % roomRows, col)
      List(pos1, pos2, pos3, pos4).filter(isNextRoomNoticeablySafe(_))
    }

    def incubation() {
      afterDelay(6) {
        sick = true
      }
      afterDelay(14) {
        dead = (1 to 25).contains(randomBelow(99) + 1)
      }

      afterDelay(16) {
        if (!dead) {
          immune = true
          sick = false
        }
      }
      afterDelay(18) {
        if (!dead) {
          immune = false
          infected = false
          sick = false
        }
      }
    }


    def move(newpos: Position) {
      row = newpos._1
      col = newpos._2
      if (!isCurrentRoomReallySafe() && !sick && !infected && !immune) {
        infected = (1 to transmissionRate).contains(randomBelow(99) + 1)
        if (infected) incubation()
      }
    }

    def tryMoving() {
      val sf: List[Position] = safeRooms()
      if (!dead && !sf.isEmpty) {
        val newpos: Position = scala.util.Random.shuffle(sf).head
        move(newpos)
      }
      afterDelay(randomBelow(5) + 1)(tryMoving)
    }

    afterDelay(randomBelow(5) + 1)(tryMoving)
  }

}
