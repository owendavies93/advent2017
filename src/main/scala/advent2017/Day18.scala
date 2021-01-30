package advent2017

import scalaadventutils.Problem

import annotation.tailrec
import scala.collection.immutable.Queue

object Day18 {

    type Machine = Map[String, Long]

    val numeric = """(-?\d+)""".r

    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day18").map(_.split(" ").toList)
        println(part1(input))
        println(countSends(input))
    }

    def part1(lines: List[List[String]]) =
        findFreq(lines, Map[String, Long]().withDefaultValue(0))

    def findFreq(lines: List[List[String]], init: Machine): Long = {
        @tailrec
        def find_(s: ProgramState): Long = {
            if (s.ptr < 0 || s.ptr >= lines.size) s.freq
            else find_(runFreq(lines(s.ptr), s))
        }

        find_(ProgramState(init, 0, 0))
    }

    def countSends(lines: List[List[String]]): Int = {
        @tailrec
        def count_(p0: MultiState, p1: MultiState): Int = {
            val p0_ = runMulti(lines(p0.ptr), p0)
            val p1_ = runMulti(lines(p1.ptr), p1)
            if ((p0_.ptr < 0 || p0_.ptr >= lines.size || p0_.blocked) &&
                (p1_.ptr < 0 || p1_.ptr >= lines.size || p1_.blocked))
                p1_.sent
            else count_(p0_, p1_)
        }

        val p0 = MultiState(
            Map("p" -> 0L).withDefaultValue(0), null)
        val p1 = MultiState(
            Map("p" -> 1L).withDefaultValue(0), p0)
        p0.other = p1

        count_(p0, p1)
    }

    private def runFreq(line: List[String], s: ProgramState) = {
        val com :: args = line
        val reg = args(0)

        com match {
            case "snd" => s.setFreq(s.get(reg)).next
            case "set" => args(1) match {
                case numeric(_) => s.update(reg, args(1).toLong).next
                case _ => s.update(reg, s.get(args(1))).next
            }
            case "rcv" => if (s.regs(reg) != 0) s.jumpTo(-1) else s.next
            case "jgz" =>
                if (s.regs(reg) > 0) s.jump(args(1).toInt) else s.next
            case _ => args(1) match {
                case numeric(_) =>
                    s.update(reg, strToOp(com)(s.regs(reg), args(1).toLong)).next
                case _ =>
                    s.update(reg, strToOp(com)(s.regs(reg), s.regs(args(1)))).next
            }
        }
    }

    private def runMulti(line: List[String], s: MultiState) = {
        val com :: args = line
        val reg = args(0)

        com match {
            case "snd" => {
                s.other.queue(s.get(reg))
                s.other.blocked = false
                s.send.next
            }
            case "set" => args(1) match {
                case numeric(_) => s.update(reg, args(1).toLong).next
                case _ => s.update(reg, s.get(args(1))).next
            }
            case "rcv" => s.take(reg).next
            case "jgz" => args(1) match {
                case numeric(_) =>
                    if (s.regs(reg) > 0) s.jump(args(1).toInt) else s.next
                case _ =>
                    if (s.regs(reg) > 0) s.jump(s.get(args(1)).toInt) else s.next
            }
            case _ => args(1) match {
                case numeric(_) =>
                    s.update(reg, strToOp(com)(s.regs(reg), args(1).toLong)).next
                case _ =>
                    s.update(reg, strToOp(com)(s.regs(reg), s.regs(args(1)))).next
            }
        }
    }

    private def strToOp(op: String): (Long, Long) => Long =
        op match {
            case "add" => _ + _
            case "mul" => _ * _
            case "mod" => _ % _
        }
}

case class ProgramState(regs: Day18.Machine, ptr: Int, freq: Long) {
    def next = copy(ptr = ptr + 1)

    def jump(value: Int) = copy(ptr = ptr + value)

    def jumpTo(value: Int) = copy(ptr = value)

    def get(reg: String) = regs(reg)

    def setFreq(value: Long) = copy(freq = value)

    def update(reg: String, value: Long) = copy(regs = regs.updated(reg, value))
}

case class MultiState
    ( regs: Day18.Machine
    , var other: MultiState
    , ptr: Int = 0
    , in: Queue[Long] = Queue.empty
    , sent: Int = 0
    , var blocked: Boolean = false) {

    def next = copy(ptr = ptr + 1)

    def jump(value: Int) = copy(ptr = ptr + value)

    def jumpTo(value: Int) = copy(ptr = value)

    def get(reg: String) = regs(reg)

    def queue(value: Long) = in.enqueue(value)

    def send = copy(sent = sent + 1)

    def take(reg: String) = {
        if (in.isEmpty)
            copy(blocked = true)
        else {
            val (elem, in_) = in.dequeue
            update(reg, elem).copy(in = in_)
        }
    }

    def update(reg: String, value: Long) = copy(regs = regs.updated(reg, value))
}

