import scala.annotation.tailrec
import scala.io.StdIn.readLine


case class StartMap(height: Int, width: Int, coordinateMap: Array[Array[Char]]) {

  case class Coordinate(x: Int, y: Int) {
    private def isValidCoordinate(coordinate: Coordinate): Boolean = {
      if (coordinate.x < 0 || coordinate.x >= width) false
      else if (coordinate.y < 0 || coordinate.y >= height) false
      else true
    }

    def neighbors: Seq[Coordinate] = {
      Seq(Coordinate(x - 1, y),
        Coordinate(x + 1, y),
        Coordinate(x, y - 1),
        Coordinate(x, y + 1))
        .filter(isValidCoordinate)
    }
  }


  def extinguishCoordinate(coordinate: Coordinate): Unit = {
    coordinateMap(coordinate.y)(coordinate.x) = '#'
  }

  def isLight(coordinate: Coordinate): Boolean = {
    coordinateMap(coordinate.y)(coordinate.x) == '-'
  }


  @tailrec
  final def extinguishStar(partOfStar: Seq[Coordinate]): Unit = {
    if (partOfStar.nonEmpty) {
      val neighboringLight = partOfStar
        .head
        .neighbors
        .filter(isLight)
      neighboringLight.foreach(extinguishCoordinate)

      extinguishStar(partOfStar.tail ++ neighboringLight)
    }
  }

  @tailrec
  final def countStars(counter: Int = 0,
                       currentCoordinate: Coordinate = Coordinate(0, 0)): Int = {

    val nextCoordinate: Option[Coordinate] = {
      if (currentCoordinate.x + 1 >= width) {
        if (currentCoordinate.y + 1 >= height) {
          None
        }
        else {
          Option(Coordinate(0, currentCoordinate.y + 1))
        }
      }
      else {
        Option(Coordinate(currentCoordinate.x + 1, currentCoordinate.y))
      }

    }


    val extraStars = if (isLight(currentCoordinate)) {
      extinguishCoordinate(currentCoordinate)
      extinguishStar(Seq(currentCoordinate))
      1
    } else {
      0
    }

    if (nextCoordinate.isEmpty) counter + extraStars
    else countStars(counter + extraStars, nextCoordinate.get)

  }


}

object CountingStars extends App {

  @tailrec
  def solveCases(caseNumber: Int = 1) : Unit = {
    val line = readLine()
    if(line != null){
      val Array(height, width) = line.split(" ").map(_.toInt)
      val coordinateMap = Array.ofDim[Char](height, width)

      for (i <- 0 until height) {
        coordinateMap(i) = readLine().toCharArray
      }

      println(s"Case $caseNumber: ${StartMap(height, width, coordinateMap).countStars()}")
      solveCases(caseNumber + 1)
    }
  }

  solveCases()
}
