package advent2017

import scalaadventutils.Problem

object Day16 {

    val spin = """s(\d+)""".r
    val exch = """x(\d+)/(\d+)""".r
    val part = """p(\w+)/(\w+)""".r

    type CharMap = Map[Char, Int]
    type PosMap  = Map[Int, Char]

    def main(args: Array[String]) {
        val comms = Problem.parseInputLineToList("day16", ",")
        val input = "abcdefghijklmnop"
        println(part1(comms, input.size - 1, input))
        println(part2(comms, input.size - 1, input))
    }

    def part1(comms: List[String], max: Int, input: String) = {
        val charMap = input.zipWithIndex.toMap
        val posMap  = charMap.keys.map(k => (charMap(k), k)).toMap
        val (_, res) = comms.foldLeft((charMap, posMap))((next, comm) =>
            parseCommand(comm, max)(next._1, next._2)
        )
        res.keys.toList.sorted.map(res(_)).mkString
    }

    def part2(comms: List[String], max: Int, input: String) = {
        val steps = Stream.iterate(input)(part1(comms, max, _))
        val cycleLength = steps.indexOf(input, 1)
        steps.drop(1000000000 % cycleLength).head
    }

    def parseCommand
        ( com: String
        , max: Int)
        : (CharMap, PosMap) => (CharMap, PosMap) = com match {

        case spin(x) => (c: CharMap, p: PosMap) => {
            val num = x.toInt
            val c_ = (0 to max).map(i => {
                if (i <= max - num) (p(i), i + num)
                else (p(i), i - (max + 1 - num))
            }).toMap

            val p_ = c_.keys.map(k => (c_(k), k)).toMap
            (c_, p_)
        }
        case exch(a, b) => (c: CharMap, p: PosMap) => {
            val iA = a.toInt
            val iB = b.toInt
            val charA = p(iA)
            val charB = p(iB)
            (c.updated(charA, iB).updated(charB, iA),
             p.updated(iB, charA).updated(iA, charB))
        }
        case part(a, b) => (c: CharMap, p: PosMap) => {
            val posA = c(a(0))
            val posB = c(b(0))
            (c.updated(a(0), posB).updated(b(0), posA),
             p.updated(posA, b(0)).updated(posB, a(0)))
        }
    }
}
