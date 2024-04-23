package ex2

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: Robot, val batteryConsumed: Int, val maxBattery: Int = 100) extends Robot:
  export robot.{position, direction}
  var battery: Int = maxBattery
  
  private def checkBattery(): Boolean =
    battery >= batteryConsumed

  private def consumeBattery(): Unit = 
    battery = battery - batteryConsumed

  override def act(): Unit =
    if this.checkBattery() then
      robot.act()
      this.consumeBattery()

  override def turn(dir: Direction): Unit = 
    if this.checkBattery() then
      robot.turn(dir)
      this.consumeBattery()

class RobotCanFail(val robot: Robot, private val probability: Double = 0.5) extends Robot:
  export robot.{position, direction}
  override def turn(dir: Direction): Unit = if Math.random() >= probability then
      robot.turn(dir)

  override def act(): Unit = if Math.random() >= probability then
      robot.act()

class RobotRepeated(val robot: Robot, private val n: Int = 1) extends Robot:
  export robot.{position, direction, turn}

  override def act(): Unit = 
    def _act(i: Int): Unit = i match
      case k if k > 0 =>
        robot.act()
        _act(i - 1)
      case _ =>
    _act(n)

@main def testRobot(): Unit =
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East  

  val randomRobot = RobotCanFail(robot)
  println("Random Robot")
  
  randomRobot.turn(randomRobot.direction.turnLeft) // Turn to North or don't
  randomRobot.act() // can be facing to North or East
  randomRobot.act() // can be facing to North or East
  randomRobot.turn(randomRobot.direction.turnLeft) //Turn to your left or don't (can be facing to North or West)
  randomRobot.act() // can be facing to North, West or East