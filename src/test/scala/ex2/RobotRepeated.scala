package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotRepeatedSpec extends AnyFlatSpec with Matchers:
  "A RobotRepeated" should "act correctly" in:
    val robot = new RobotRepeated(SimpleRobot((0, 0), Direction.North), n = 2)

    robot.turn(Direction.South)
    robot.act()
    robot.direction shouldBe Direction.South
    robot.position shouldBe (0, -2)

    robot.turn(Direction.North)
    robot.act()
    robot.direction shouldBe Direction.North
    robot.position shouldBe (0, 0)

    robot.turn(Direction.West)
    robot.act()
    robot.direction shouldBe Direction.West
    robot.position shouldBe (-2, 0)

    robot.turn(Direction.East)
    robot.act()
    robot.direction shouldBe Direction.East
    robot.position shouldBe (0, 0)

  it should "act normally with n = 1" in:
    val robot = new RobotRepeated(SimpleRobot((0, 0), Direction.North))

    robot.act()
    robot.position shouldBe (0, 1)
    robot.direction shouldBe Direction.North

    robot.turn(Direction.East)
    robot.act()
    robot.position shouldBe (1, 1)
    robot.direction shouldBe Direction.East

    robot.turn(Direction.South)
    robot.act()
    robot.position shouldBe (1, 0)
    robot.direction shouldBe Direction.South

    robot.turn(Direction.West)
    robot.act()
    robot.position shouldBe (0, 0)
    robot.direction shouldBe Direction.West