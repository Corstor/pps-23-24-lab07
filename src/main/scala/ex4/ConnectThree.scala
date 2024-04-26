package ex4

import java.util.OptionalInt

// Optional!
object ConnectThree extends App:
  val bound = 3
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X

  case class Disk(x: Int, y: Int, player: Player)
  /**
   * Board:
   * y
   *
   * 3
   * 2
   * 1
   * 0
   *   0 1 2 3 <-- x
   */
  type Board = Seq[Disk]
  type Game = Seq[Board]

  import Player.*

  def find(board: Board, x: Int, y: Int): Option[Player] = board match
    case h :: t => if h.x == x && h.y == y then Option(h.player) else find(t, x, y)
    case _ => Option.empty

  def firstAvailableRow(board: Board, x: Int): Option[Int] =
    def _firstAvailableRow(board: Board, x: Int, c: Int): Option[Int] = board match
      case h :: t => _firstAvailableRow(t, x, c + (if h.x == x then 1 else 0))
      case _ => if c > bound then Option.empty else Option(c)
    _firstAvailableRow(board, x, 0)

  def placeAnyDisk(board: Board, player: Player): Seq[Board] =
    for
      x <- 0 to bound
      y <- firstAvailableRow(board,x)
      disk = Disk(x, y, player)
    yield
      disk +: board

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] =
    val emptyGame: LazyList[Game] = LazyList(List(List()))
    def _computeAnyGame(player: Player, moves: Int): LazyList[Game] =
      moves match
        case 0 => emptyGame
        case _ =>
          for
            game <- _computeAnyGame(player.other, moves - 1)
            sol <- placeAnyDisk(game.head, player)
          yield
            if !isWon(sol) then sol +: game else game
    _computeAnyGame(if moves % 2 == 0 then player.other else player, moves)
  
  private def isWon(sol: Board): Boolean = 
    (for
      x <- 0 to bound
      y <- 0 to bound
      if !((x == 0 || x == bound) && (y == 0 || y == bound))
      player <- find(sol, x, y)
      if isConnectedToThree(x, y, player, sol)
    yield
      true).nonEmpty

  private def isConnectedToThree(x: Int, y: Int, player: Player, board: Board): Boolean =
    (board.find(_ == Disk(x - 1, y, player)).nonEmpty && board.find(_ == Disk(x + 1, y, player)).nonEmpty) ||
    (board.find(_ == Disk(x, y - 1, player)).nonEmpty && board.find(_ == Disk(x, y + 1, player)).nonEmpty) ||
    (board.find(_ == Disk(x - 1, y - 1, player)).nonEmpty && board.find(_ == Disk(x + 1, y + 1, player)).nonEmpty) ||
    (board.find(_ == Disk(x - 1, y + 1, player)).nonEmpty && board.find(_ == Disk(x + 1, y - 1, player)).nonEmpty)

  def printBoards(game: Seq[Board]): Unit =
    for
      y <- bound to 0 by -1
      board <- game.reverse
      x <- 0 to bound
    do
      print(find(board, x, y).map(_.toString).getOrElse("."))
      if x == bound then
        print(" ")
        if board == game.head then println()

  // Exercise 1: implement find such that..
  println("EX 1: ")
  println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
  println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

//   // Exercise 2: implement firstAvailableRow such that..
  println("EX 2: ")
  println(firstAvailableRow(List(), 0)) // Some(0)
  println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
  println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None
  // Exercise 2: implement placeAnyDisk such that..
  printBoards(placeAnyDisk(List(), X))
  // .... .... .... ....
  // .... .... .... ....
  // .... .... .... ....
  // ...X ..X. .X.. X...
  printBoards(placeAnyDisk(List(Disk(3, 0, O)), X))
  // .... .... .... ....
  // .... .... .... ....
  // ...X .... .... ....
  // ...O ..XO .X.O X..O
  println("EX 4: ")
// Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(O, 17).foreach ( g =>
    printBoards(g)
    println()
  )

//  .... .... .... .... ...O
//  .... .... .... ...X ...X
//  .... .... ...O ...O ...O
//  .... ...X ...X ...X ...X
//
//
// .... .... .... .... O...
// .... .... .... X... X...
// .... .... O... O... O...
// .... X... X... X... X...

// Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
