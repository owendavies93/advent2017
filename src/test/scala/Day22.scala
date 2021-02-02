package advent2017

import org.scalatest.funsuite.AnyFunSuite

class Day22Spec extends AnyFunSuite {
    val lines = List[String](
        "..#",
        "#..",
        "..."
    )

    val grid = Day22.constructGrid(lines)
    val start = (Pnt(lines.size / 2, lines(0).size / 2), Direction.U)

    test("Day 22: part1") {
        assert(Day22.part1(lines, grid, start, 7) == 5)
        assert(Day22.part1(lines, grid, start, 70) == 41)
        assert(Day22.part1(lines, grid, start, 10000) == 5587)
    }

    test("Day 22: part2") {
        assert(Day22.part2(lines, grid, start, 100) == 26)
        assert(Day22.part2(lines, grid, start, 10000000) == 2511944)
    }
}
