package advent2017

object Day15 {

    def main(args: Array[String]) {
        println(part1(40000000, 516, 190))
        println(part2(5000000,  516, 190))
    }

    def part1(pairs: Int, a: Int, b: Int) = {
        val genA = generator(a, 16807)
        val genB = generator(b, 48271)

        genA.take(pairs + 1) zip genB.take(pairs + 1) count {
            case (a_, b_) => compare16Bits(a_, b_)
        }
    }

    def part2(pairs: Int, a: Int, b: Int) = {
        val genA = generator(a, 16807)
        val genB = generator(b, 48271)

        genA.filter(_ % 4 == 0).take(pairs + 1) zip
        genB.filter(_ % 8 == 0).take(pairs + 1) count {
            case (a_, b_) => compare16Bits(a_, b_)
        }
    }

    def generator(start: Int, factor: Int) =
        Iterator.iterate(start)(res =>
            (res.toLong * factor % 2147483647).toInt
        ).drop(1)

    private def compare16Bits(a: Int, b: Int) =
        (a & (1 << 16) - 1) == (b & (1 << 16) - 1)
}
