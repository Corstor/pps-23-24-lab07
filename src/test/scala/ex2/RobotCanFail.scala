package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotCanFailSpec extends AnyFlatSpec with Matchers:
  "A RobotCanFail" should "not fail with 0 prob" in:
    val robot = new RobotCanFail(SimpleRobot((0, 0), Direction.North), probability = 0)

    robot.turn(Direction.East)
    robot.act()
    robot.direction should be(Direction.East)
    robot.position should be((1, 0))

    robot.turn(Direction.South)
    robot.act()
    robot.direction should be(Direction.South)
    robot.position should be((1, -1))

    robot.turn(Direction.West)
    robot.act()
    robot.direction should be(Direction.West)
    robot.position should be((0, -1))

    robot.turn(Direction.North)
    robot.act()
    robot.direction should be(Direction.North)
    robot.position should be((0, 0))

  it should "always fail with 1 prob" in:
    val robot = new RobotCanFail(SimpleRobot((0, 0), Direction.North), probability = 1)

    robot.act()
    robot.position should be((0, 0))
    robot.direction should be(Direction.North)

    robot.turn(Direction.East)
    robot.act()
    robot.position should be((0, 0))
    robot.direction should be(Direction.North)

    robot.turn(Direction.South)
    robot.act()
    robot.position should be((0, 0))
    robot.direction should be(Direction.North)

    robot.turn(Direction.West)
    robot.act()
    robot.position should be((0, 0))
    robot.direction should be(Direction.North)