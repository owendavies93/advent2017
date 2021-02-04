package advent2017

import scalaadventutils.Problem

import scala.collection.mutable.ListBuffer

object Day24 {

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day24")

        println(findStrongestAndLongest(parseInput(lines)))
    }

    def findStrongestAndLongest(cs: List[Component]): (Int, Int) = {
        val bridges = ListBuffer[List[Component]]()

        def find_
            ( current: Component
            , usedSock: Socket
            , currentBridge: ListBuffer[Component]
            , used: collection.mutable.Set[Component]) {

            val otherSock = current.otherSocket(usedSock)
            val candidates = findMatchingComponents(otherSock, cs)
                                .filterNot(_ == current)
                                .filterNot(used.contains(_))

            if (candidates.isEmpty || candidates.forall(used.contains(_))) {
                bridges += currentBridge.toList
                return
            }

            used += current

            for (c <- candidates) {
                if (!used.contains(c)) {
                    currentBridge += c
                    find_(c, otherSock, currentBridge, used)
                    currentBridge -= c
                }
            }

            used -= current
        }

        findMatchingComponents(Socket(0), cs).map(s =>
            find_(s, Socket(0), ListBuffer(s), collection.mutable.Set[Component](s))
        )

        val bs = bridges.toList
        val maxLength = bs.maxBy(_.size).size

        val strongest = bs.map(b => b.map(_.strength).sum).max
        val longest = bs.filter(_.size == maxLength).map(b => b.map(_.strength).sum).max
        (strongest, longest)
    }

    def findMatchingComponents(s: Socket, cs: List[Component]) =
        cs.filter(_.fits(s))

    def parseInput(lines: List[String]) =
        lines.map(l => Component(l.split("/").map(s => Socket(s.toInt)).toList))

    case class Component(sockets: List[Socket]) {
        def fits(s: Socket) = sockets.contains(s)

        def otherSocket(s: Socket) = {
            val other = sockets.filterNot(_ == s)
            if (other.isEmpty) s else other(0)
        }

        def strength = sockets.map(_.pins).sum

        override def toString() = sockets.mkString("/")
    }

    case class Socket(pins: Int) {
        override def equals(that: Any) = that match {
            case s:Socket => pins == s.pins
            case _ => false
        }

        override def hashCode = pins.##

        override def toString() = pins.toString()
    }

}
