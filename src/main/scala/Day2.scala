import scala.annotation.tailrec
import scala.io.Source

@main
def Day2(fileName: String, part: Int): Unit = {
  import Day2Solution._

  if ((fileName != "input" && fileName != "test") || part < 1 || part > 2)
    println("Wrong file name")
  else {
    val source      = Source.fromResource(s"day2/$fileName.txt")
    val input       = source.getLines().toList
    val parsedInput = parseInput(input)
    val result      = if (part == 1) part1(parsedInput) else part2(parsedInput)

    println(result)

    source.close()
  }
}

object Day2Solution {
  def parseInput(lines: List[String]): List[List[Int]] =
    lines.map(_.split(" ").map(_.toInt).toList)

  def part1(reports: List[List[Int]]): Int =
    reports.count { report =>
      closeAdjacentLevels(report) && allIncreasingOrAllDecreasing(report)
    }

  def part2(reports: List[List[Int]]): Int = {
    @tailrec
    def checkSubReports(subReport: List[Int], originalReport: List[Int], currentIndexRemoved: Int): Boolean = {
      val result           = closeAdjacentLevels(subReport) && allIncreasingOrAllDecreasing(subReport)
      val nextRemovedIndex = currentIndexRemoved + 1

      if (result)
        result
      else if (nextRemovedIndex >= originalReport.size)
        false
      else
        checkSubReports(originalReport.patch(nextRemovedIndex, Nil, 1), originalReport, nextRemovedIndex)
    }

    reports.count { report =>
      val result = closeAdjacentLevels(report) && allIncreasingOrAllDecreasing(report)

      if (result)
        result
      else
        checkSubReports(report.tail, report, 0)
    }
  }

  private def closeAdjacentLevels(report: List[Int]): Boolean =
    report.sliding(2).forall {
      case List(left, right) =>
        val offset = math.abs(left - right)
        offset >= 1 && offset <= 3

      case _ =>
        true
    }

  private def allIncreasingOrAllDecreasing(report: List[Int]): Boolean =
    report.sorted == report || report.sortBy(-_) == report
}
