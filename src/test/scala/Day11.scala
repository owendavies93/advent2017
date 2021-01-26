package advent2017

import org.scalatest.funsuite.AnyFunSuite

class Day11Spec extends AnyFunSuite {

    test("Day 11: stepsAway") {
        var dirs = List[String]("ne","ne","ne")
        var end = Day11.getEndpoint(dirs)

        assertResult(3) {
            Day11.stepsAway(end)
        }

        dirs = List[String]("ne","ne","sw","sw")
        end = Day11.getEndpoint(dirs)

        assertResult(0) {
            Day11.stepsAway(end)
        }

        dirs = List[String]("ne","ne","s","s")
        end = Day11.getEndpoint(dirs)

        assertResult(2) {
            Day11.stepsAway(end)
        }

        dirs = List[String]("se","sw","se","sw","sw")
        end = Day11.getEndpoint(dirs)

        assertResult(3) {
            Day11.stepsAway(end)
        }
    }
}
