package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotWithBatterySpec extends AnyFlatSpec with Matchers:
  "A RobotWithBattery" should "turn correctly" in:
    val robot = new RobotWithBattery(SimpleRobot((0, 0), Direction.North), batteryConsumed = 5)

    robot.battery shouldBe robot.maxBattery

    robot.turn(Direction.East)
    robot.direction should be(Direction.East)
    robot.battery shouldBe robot.maxBattery - robot.batteryConsumed

    robot.turn(Direction.South)
    robot.direction should be(Direction.South)
    robot.battery shouldBe robot.maxBattery - robot.batteryConsumed * 2

    robot.turn(Direction.West)
    robot.direction should be(Direction.West)
    robot.battery shouldBe robot.maxBattery - robot.batteryConsumed * 3

    robot.turn(Direction.North)
    robot.direction should be(Direction.North)
    robot.battery shouldBe robot.maxBattery - robot.batteryConsumed * 4

  it should "act correctly" in:
    val robot = new RobotWithBattery(SimpleRobot((0, 0), Direction.North), batteryConsumed = 5)

    robot.act()
    robot.position should be((0, 1))
    robot.battery shouldBe robot.maxBattery - robot.batteryConsumed

    robot.turn(Direction.East)
    robot.act()
    robot.position should be((1, 1))
    robot.battery shouldBe robot.maxBattery - robot.batteryConsumed * 3

    robot.turn(Direction.South)
    robot.act()
    robot.position should be((1, 0))
    robot.battery shouldBe robot.maxBattery - robot.batteryConsumed * 5

    robot.turn(Direction.West)
    robot.act()
    robot.position should be((0, 0))
    robot.battery shouldBe robot.maxBattery - robot.batteryConsumed * 7

  it should "not work without battery" in:
    val robot = new RobotWithBattery(SimpleRobot((0, 0), Direction.North), batteryConsumed = 50)

    robot.turn(Direction.East)
    robot.act()
    robot.battery shouldBe 0

    robot.position shouldBe (1, 0)
    robot.act()
    robot.position shouldBe (1, 0)

    robot.turn(Direction.South)
    robot.direction shouldBe Direction.East