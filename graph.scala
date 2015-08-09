// Directed graph and Dijkstra shortest path for solving Euler 81,82,83
object Graph {
  // class to contain an adjacency list
  // built from an array of N nodes
  // points to a List of (weight, node) pairs for graph edges
  // nodes have implicit 0...N-1 labels
  class AdjacencyList(val edges: Array[List[DirectedEdge]]) {
    val N = edges.length
  }

  // read matrix file and constuct directed graph with allowed transitions
  object AdjacencyList {
    def apply(matrixFile: String, 
      N: Int,
      allowLeft: Boolean = true,
      allowRight: Boolean = true,
      allowUp: Boolean = true,
      allowDown: Boolean = true,
      withSides: Boolean = false
    ) = {
      val edges = Array.fill(N*N)(List[DirectedEdge]())

      // to accomodate euler82, create left and right nodes connecting to the left and right sides
      val leftSource = collection.mutable.Buffer[DirectedEdge]()
      val rightSink = collection.mutable.Buffer[DirectedEdge]()

      val f = scala.io.Source.fromFile(matrixFile)

      var i = 0
      f.getLines.foreach{line => 
        var j = 0
        line.split(",").foreach{weight =>
          // now apply a path to this point with this weight from all allowed nodes
          val node = i*N + j

          val right = i*N + (j+1)
          val left = i*N+ (j-1)
          val up = (i-1)*N + j
          val down = (i+1)*N + j

          val edge = DirectedEdge(weight.toInt, node)

          if (allowRight && j-1 >= 0) edges(left) = edge +: edges(left)
          if (allowLeft && j+1 < N) edges(right) = edge +: edges(right)
          if (allowUp && i+1 < N) edges(down) = edge +: edges(down)
          if (allowDown && i-1 >= 0) edges(up) = edge +: edges(up)

          if (j == 0) leftSource += edge
          if (withSides && j == N-1) {
            edges(node) = DirectedEdge(0, N*N + 1) +: edges(node)
          }

          j += 1
        }
        i += 1
      }

      if (withSides) 
        new AdjacencyList(edges ++ Array(leftSource.toList, rightSink.toList))
      else
        new AdjacencyList(edges)
    }
  }

  case class DirectedEdge(weight: Int, to: Int) {
    assert(weight >= 0.0, "Weight must be non-negative!")
  }

  def shortestPath(g: AdjacencyList, s: Int, t: Int, offset: Int = 0) {
    // Djikstra's algorithm
    // assertions on s, t, N etc
    
    // initialize
    val candidateDistance = Array.fill(g.N)(Int.MaxValue)
    val visited = Array.fill(g.N)(false)

    candidateDistance(s) = offset

    while(!visited(t)) {
      val n = indexOfMinUnvisited(candidateDistance, visited)
      val distanceToN = candidateDistance(n)

      g.edges(n).foreach{ 
        case DirectedEdge(w, m) if !visited(m) =>
          if (distanceToN + w < candidateDistance(m)){
            candidateDistance(m) = distanceToN + w
          }

        case _ => 
      }

      visited(n) = true
    }

    println(s"Shortest path to ${t} is ${candidateDistance(t)}")
  }

  def indexOfMinUnvisited(a: Array[Int], visited: Array[Boolean]) = {
    var min = Int.MaxValue
    var location = -1

    (0 until a.length).foreach{ i =>
      if (!visited(i)) {
        val cur = a(i)

        if (cur < min) {
          location = i
          min = cur
        }
      }
    }

    location
  }

  def euler81() {
    // booleans allow only left and down transitions
    val g = AdjacencyList("p081_matrix.txt", 80, false, true, false, true)
    shortestPath(g, 0, 6399, 4445)
  }

  def euler82(file: String = "p081_matrix.txt", N: Int = 80) = {
    // left, right, down transitions and dummy nodes on sides
    val g = AdjacencyList(file, N, false, true, true, true, withSides = true)
    shortestPath(g, N*N, N*N+1)
  }

  def euler83() {
    val g = AdjacencyList("p081_matrix.txt", 80)
    shortestPath(g, 0, 6399, 4445) // offset by the top left element
  }
}

object Euler81 { def apply() { Graph.euler81() } }
object Euler82 { def apply() { Graph.euler82() } }
object Euler83 { def apply() { Graph.euler83() } }
