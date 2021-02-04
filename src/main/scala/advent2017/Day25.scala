package advent2017

import scalaadventutils.Profiler.timeMS

object Day25 {

    type Tape = Map[Int, Int]

    def main(args: Array[String]) {
        println(part1)
    }

    def part1 =
        Iterator.iterate(("A", 0, Map[Int, Int]().withDefaultValue(0)))(res => {
            val (state, pos, tape) = res
            machine(state, pos, tape)
        }).drop(12134526).next()._3.values.count(_ == 1)

    def machine(state: String, pos: Int, t: Tape): (String, Int, Tape) =
        state match {
            case "A" =>
                if (t(pos) == 0) ("B", pos + 1, t.updated(pos, 1))
                else ("C", pos - 1, t.updated(pos, 0))
            case "B" =>
                if (t(pos) == 0) ("A", pos - 1, t.updated(pos, 1))
                else ("C", pos + 1, t.updated(pos, 1))
            case "C" =>
                if (t(pos) == 0) ("A", pos + 1, t.updated(pos, 1))
                else ("D", pos - 1, t.updated(pos, 0))
            case "D" =>
                if (t(pos) == 0) ("E", pos - 1, t.updated(pos, 1))
                else ("C", pos - 1, t.updated(pos, 1))
            case "E" =>
                if (t(pos) == 0) ("F", pos + 1, t.updated(pos, 1))
                else ("A", pos + 1, t.updated(pos, 1))
            case "F" =>
                if (t(pos) == 0) ("A", pos + 1, t.updated(pos, 1))
                else ("E", pos + 1, t.updated(pos, 1))
        }
}
