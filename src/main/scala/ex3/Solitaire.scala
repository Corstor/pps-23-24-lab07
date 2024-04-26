package ex3

object Solitaire extends App:
  type Position = (Int, Int)
  type Solution = Iterable[Position]
  type IterableFactory = Solution => Iterable[Solution]
  val width = 5
  val height = 7
  val startMark = (width / 2 + 1, height / 2 + 1)
  given IterableFactory = LazyList(_)

  def placeMarks(w: Int = width, h: Int = height, counter: Int = 1)(marks: Iterable[Position] = List(startMark), lastMark: Position = startMark)(using factory: IterableFactory): Iterable[Solution] = counter match
    case n if n == w * h => if marks.size == counter then factory(marks) else factory(Set())
    case _ =>
      for
        x <- lastMark._1 - 3 to lastMark._1 + 3
        y <- lastMark._2 - 3 to lastMark._2 + 3
        if x > 0 && x <= w
        if y > 0 && y <= h
        mark = (x, y)
        if isSafe(mark, marks) && canMoveFrom(lastMark, mark)
        newMarks <- placeMarks(counter = counter + 1)(mark +: marks.toSeq, mark)
      yield
        newMarks

    def isSafe(mark: Position, marks: Iterable[Position]): Boolean =
      !marks.toSeq.contains(mark)
    
    def canMoveFrom(last: Position, mark: Position): Boolean =
    ((mark._2 - last._2).abs == (mark._1 - last._1).abs && (mark._1 - last._1).abs == 2) ||
    ((mark._1 - last._1).abs == 3 && mark._2 == last._2) ||
    ((mark._2 - last._2).abs == 3 && mark._1 == last._1)
  
  def render(solution: Seq[Position], width: Int = width, height: Int = height): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 to height
          row = for x <- 0 to width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  val solutions = placeMarks()()
  solutions foreach ((sol) => println(render(sol.toSeq)))
  println("Number of solutions: " + solutions.size)