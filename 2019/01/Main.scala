import scala.io.Source


object Main {

  def main(args: Array[String]) = {
    val xs = Source.fromFile(args(0))
                   .getLines()
                   .filter(!_.isEmpty())
                   .map(line => line.toInt)
                   .toList

    val step = (x: Int) => x / 3 - 2

    val p1 = xs.map(step).sum
    println(s"Part 1: $p1")

    val p2 = xs.map(Iterator.iterate(_)(step).takeWhile(_ > 0).drop(1).sum).sum
    println(s"Part 2: $p2")
  }
}
