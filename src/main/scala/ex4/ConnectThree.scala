package ex4

import java.util.OptionalInt
import scala.util.Random

trait ConnectThree:
  enum Player:
    case X, O
    def other: Player = this match
      case X => O
      case _ => X
  type Board
  type Game
  def find(board: Board, x: Int, y: Int): Option[Player]
  def firstAvailableRow(board: Board, x: Int): Option[Int]
  def placeAnyDisk(board: Board, player: Player): Seq[Board]
  def computeAnyGame(player: Player, moves: Int): LazyList[Game]
  def randomAI(board: Board, player: Player): Board
  def smartAI(board: Board, player: Player): Board
  def putDisk(board: Board, x: Int, player: Player): Board
  def boardIsFull(board: Board): Boolean
  def isWon(board: Board): Option[Player]

object ConnectThree extends App:
  def apply(): ConnectThree = ConnectThreeImpl()
  ConnectThreeImpl().main() //Called just to run the print inside the ConnectThreeImpl class

  private class ConnectThreeImpl extends ConnectThree:
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
    import PrivateMethods.*

    override def find(board: Board, x: Int, y: Int): Option[Player] = board match
      case h :: t => if h.x == x && h.y == y then Option(h.player) else find(t, x, y)
      case _ => Option.empty

    override def firstAvailableRow(board: Board, x: Int): Option[Int] =
      def _firstAvailableRow(board: Board, x: Int, c: Int): Option[Int] = board match
        case h :: t => _firstAvailableRow(t, x, c + (if h.x == x then 1 else 0))
        case _ => if c > bound then Option.empty else Option(c)
      _firstAvailableRow(board, x, 0)

    override def placeAnyDisk(board: Board, player: Player): Seq[Board] =
      for
        x <- 0 to bound
        y <- firstAvailableRow(board,x)
        disk = Disk(x, y, player)
      yield
        disk +: board

    override def computeAnyGame(player: Player, moves: Int): LazyList[Game] =
      val emptyGame: LazyList[Game] = LazyList(List(List()))
      def _computeAnyGame(player: Player, moves: Int): LazyList[Game] =
        moves match
          case 0 => emptyGame
          case _ =>
            for
              game <- _computeAnyGame(player.other, moves - 1)
              sol <- placeAnyDisk(game.head, player)
            yield
              if isWon(sol).isEmpty then sol +: game else game
      _computeAnyGame(if moves % 2 == 0 then player.other else player, moves)

    override def boardIsFull(board: Board): Boolean =
      board.size == (bound + 1) * (bound + 1)

    override def randomAI(board: Board, player: Player): Board =
      if boardIsFull(board) then
        board
      else
        val x = Random.nextInt(bound)
        val boardWithRandomDisk = putDisk(board, x, player)
        if boardWithRandomDisk == board then
          randomAI(board, player)
        else
          boardWithRandomDisk

    override def smartAI(board: Board, player: Player): Board = 
      val consecutiveDisksWithColumn = for
        x <- 0 to bound
        disks = diskInAColumn(board, x)
      yield
        (consecutiveDisks(disks, player), x)

      val nDisks = consecutiveDisksWithColumn.sortBy(d => d._1).filter(d => firstAvailableRow(board, d._2).nonEmpty)
      if nDisks.nonEmpty then
        val maxDisks = nDisks.last
        val x = maxDisks._2
        putDisk(board, x, player)
      else
        board

    override def putDisk(board: Board, x: Int, player: Player): Board = 
      val y = firstAvailableRow(board, x)
      if y.isEmpty then
        board
      else
        Disk(x, y.get, player) +: board

    override def isWon(sol: Board): Option[Player] =
      val winningPlayer = for
        x <- 0 to bound
        y <- 0 to bound
        if !((x == 0 || x == bound) && (y == 0 || y == bound))
        player <- find(sol, x, y)
        if isConnectedToThree(x, y, player, sol)
      yield
        player
      if winningPlayer.isEmpty then Option.empty else Option(winningPlayer.last)

    private object PrivateMethods:
      val bound = 3

      def isConnectedToThree(x: Int, y: Int, player: Player, board: Board): Boolean =
        (board.find(_ == Disk(x - 1, y, player)).nonEmpty && board.find(_ == Disk(x + 1, y, player)).nonEmpty) ||
        (board.find(_ == Disk(x, y - 1, player)).nonEmpty && board.find(_ == Disk(x, y + 1, player)).nonEmpty) ||
        (board.find(_ == Disk(x - 1, y - 1, player)).nonEmpty && board.find(_ == Disk(x + 1, y + 1, player)).nonEmpty) ||
        (board.find(_ == Disk(x - 1, y + 1, player)).nonEmpty && board.find(_ == Disk(x + 1, y - 1, player)).nonEmpty)

      def diskInAColumn(board: Board, x: Int): Seq[Disk] =
      board.filter(d => d._1 == x)

      def consecutiveDisks(disks: Seq[Disk], player: Player): Int = 
        disks.foldRight(0, player)((disk, c) =>
          if disk._3 == player && c._2 == player then
            (c._1 + 1, c._2)
          else
            (c._1, player.other)
        )._1

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

    def main() =
      println("EX 1: ")
      println(find(List(Disk(0, 0, X)), 0, 0)) // Some(X)
      println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 0, 1)) // Some(O)
      println(find(List(Disk(0, 0, X), Disk(0, 1, O), Disk(0, 2, X)), 1, 1)) // None

      println("EX 2: ")
      println(firstAvailableRow(List(), 0)) // Some(0)
      println(firstAvailableRow(List(Disk(0, 0, X)), 0)) // Some(1)
      println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X)), 0)) // Some(2)
      println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X)), 0)) // Some(3)
      println(firstAvailableRow(List(Disk(0, 0, X), Disk(0, 1, X), Disk(0, 2, X), Disk(0, 3, X)), 0)) // None

      println:
        "'Smart' AI"
      printBoards:
        List(smartAI(List(Disk(0, 0, O), Disk(1, 0, O), Disk(2, 0, X)), X))
      
      printBoards(placeAnyDisk(List(), X))
      printBoards(placeAnyDisk(List(Disk(3, 0, O)), X))

      println("EX 4: ")
    
      computeAnyGame(O, 5).foreach ( g =>
        printBoards(g)
        println()
      )
