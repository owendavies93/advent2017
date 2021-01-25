package advent2017

import org.scalatest.funsuite.AnyFunSuite

class Day9Spec extends AnyFunSuite {

    test("Day 9: score") {
        var input = "{}"

        assertResult(1) {
            Day9.score(input)
        }

        input = "{{{}}}"

        assertResult(6) {
            Day9.score(input)
        }

        input = "{{},{}}"

        assertResult(5) {
            Day9.score(input)
        }

        input = "{{{},{},{{}}}}"

        assertResult(16) {
            Day9.score(input)
        }

        input = "{<a>,<a>,<a>,<a>}"

        assertResult(1) {
            Day9.score(input)
        }

        input = "{{<ab>},{<ab>},{<ab>},{<ab>}}"

        assertResult(9) {
            Day9.score(input)
        }

        input = "{{<!!>},{<!!>},{<!!>},{<!!>}}"

        assertResult(9) {
            Day9.score(input)
        }

        input = "{{<a!>},{<a!>},{<a!>},{<ab>}}"

        assertResult(3) {
            Day9.score(input)
        }
    }

    test("Day 9: countGarbage") {
        var input = "<>"

        assertResult(0) {
            Day9.countGarbage(input)
        }

        input = "<random characters>"

        assertResult(17) {
            Day9.countGarbage(input)
        }

        input = "<<<<>"

        assertResult(3) {
            Day9.countGarbage(input)
        }

        input = "<{!>}>"

        assertResult(2) {
            Day9.countGarbage(input)
        }

        input = "<!!>"

        assertResult(0) {
            Day9.countGarbage(input)
        }

        input = "<!!!>>"

        assertResult(0) {
            Day9.countGarbage(input)
        }

        input = "<{o\"i!a,<{i<a>"

        assertResult(10) {
            Day9.countGarbage(input)
        }
    }
}
