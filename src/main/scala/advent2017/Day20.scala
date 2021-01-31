package advent2017

import scalaadventutils.Problem

import scala.math._

object Day20 {

    val parser = """p=<(-?\d+),(-?\d+),(-?\d+)>, v=<(-?\d+),(-?\d+),(-?\d+)>, a=<(-?\d+),(-?\d+),(-?\d+)>""".r

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day20")

        part1(lines)
        part2(lines)
    }

    def part1(lines: List[String]) = {
        val particles = parse(lines)

        (1 to 500).foldLeft(particles)((next, i) => {
            val nearest = next.sortBy(_.distance).head

            if (i % 100 == 0) println(nearest)

            next.map(_.tick)
        })
    }

    def part2(lines: List[String]) = {
        val particles = parse(lines).toList

        (1 to 100).foldLeft(particles)((next, i) => {
            val ps = removeCollisions(next)

            if (i % 100 == 0) println(ps.size)

            ps.map(_.tick)
        })
    }

    private def parse(lines: List[String]) =
        lines.zipWithIndex.map(l => {
            val (p, i) = l
            p match {
                case parser(pX, pY, pZ, vX, vY, vZ, aX, aY, aZ) =>
                    Particle(pX.toInt, pY.toInt, pZ.toInt, i,
                             new Vector(vX.toInt, vY.toInt, vZ.toInt),
                             new Vector(aX.toInt, aY.toInt, aZ.toInt))
            }
        })

    private def removeCollisions(ps: List[Particle]) = {
        val dupes = ps.groupBy(p => (p.x, p.y, p.z))
                      .filter(_._2.size > 1)
                      .flatMap(_._2).toSet
        (ps.toSet -- dupes).toList
    }

}

class Vector(val x: Int, val y: Int, val z: Int) {
    def add(v: Vector) = new Vector(x + v.x, y + v.y, z + v.z)
}

case class Particle
    ( override val x: Int
    , override val y: Int
    , override val z: Int
    , id: Int
    , vel: Vector
    , acc: Vector) extends Vector(x, y, z) {

    def distance = abs(x) + abs(y) + abs(z)

    def tick = {
        val vel_ = vel.add(acc)
        Particle(x + vel_.x, y + vel_.y, z + vel_.z, id, vel_, acc)
    }

    override def toString() = id.toString()
}

