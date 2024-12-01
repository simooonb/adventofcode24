import scala.io.Source

@main
def Day1(fileName: String, part: Int): Unit =
  if ((fileName != "input" && fileName != "test") || part < 1 || part > 2)
    println("Wrong file name")
  else {
    val source = Source.fromResource(s"day1/$fileName.txt")
    val input  = source.getLines().toList
    val result = if (part == 1) part1(input) else part2(input)

    println(result)

    source.close()
  }

def part1(lines: List[String]): Int = {
  val (lefts, rights) = lines.map { line =>
    val Array(left, right) = line.split(" {3}")
    (left.toInt, right.toInt)
  }.unzip

  (lefts.sorted zip rights.sorted).foldLeft(0) { case (acc, (l, r)) =>
    acc + math.abs(l - r)
  }
}

def part2(lines: List[String]): Int = {
  val (lefts, rights) = lines.map { line =>
    val Array(left, right) = line.split(" {3}")
    (left.toInt, right.toInt)
  }.unzip

  val rightCache = rights.map(i => i -> rights.count(_ == i)).toMap

  lefts.foldLeft(0)((acc, i) => acc + i * rightCache.getOrElse(i, 0))
}
