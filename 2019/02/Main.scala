import scala.io.Source
import scala.annotation.tailrec
import scala.util.Try


object Main {

  def update(xs: Vector[Int], ip: Int, func: (Int, Int) => Int): Vector[Int] = {
    val (op1, op2, dst) = (xs(xs(ip + 1)), xs(xs(ip + 2)), xs(ip + 3))
    xs.updated(dst, func(op1, op2))
  }

  @tailrec
  def step(xs: Vector[Int], ip: Int): Vector[Int] = {
    xs(ip) match {
      case 1  => step(update(xs, ip, _ + _), ip + 4)
      case 2  => step(update(xs, ip, _ * _), ip + 4)
      case 99 => xs // done
      case _  => throw new IllegalArgumentException(s"${xs(0)}")
    }
  }

  def part2(xs: Vector[Int]): Int = {
    for (noun <- 0 to 99; verb <- 0 to 99) {
      Try {
        val res = step(xs.updated(1, noun).updated(2, verb), 0).head
        if (res == 19690720)
          return 100 * noun + verb
      }
    }

    return -1
  }

  def main(args: Array[String]) = {
    var xs = Source.fromFile(args(0))
                   .getLines()
                   .next()
                   .split(",")
                   .map(_.toInt)
                   .toVector

    val p1 = step(xs.updated(1, 12).updated(2, 2), 0).head
    println(s"Part 1: $p1")

    println(s"Part 2: ${part2(xs)}")
  }
}
