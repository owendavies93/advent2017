package advent2017

import scalaadventutils.CircularList

import annotation.tailrec

object Day17 {

    def main(args: Array[String]) {
        println(part1)
        println(part2)
    }

    def part1 = spinLock(343)

    def part2 = trackPosition(343)

    /*
        0 will always be at index 0 in the list, so we only need
        to keep track of values at index 1 in the list. Return the
        last value set at index 1 when the target size is reached.
    */
    def trackPosition(step: Int) = {
        @tailrec
        def track_(size: Int, index: Int, value: Int): Int = {
            if (size == 50000000) value
            else {
                val newIndex = ((index + step) % size) + 1
                val newValue = if (newIndex == 1) size else value
                track_(size + 1, newIndex, newValue)
            }
        }

        track_(1, 0, 0)
    }

    def spinLock(step: Int) = {
        val result = (1 to 2017).foldLeft((CircularList(2018)(0), 0))(
            (next, value) => {
                val (list, index) = next
                val newIndex = ((index + step) % list.size) + 1
                (list.insertAt(newIndex, value), newIndex)
            }
        )._1.queue

        val index = result.indexOf(2017) + 1
        result(index)
    }
}
