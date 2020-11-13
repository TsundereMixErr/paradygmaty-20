object l4 {
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

  //ZADANIE 1 (3pkt)
  def createTree(depth: Int, from: Int, to: Int): BT[Int] = {
    val r = scala.util.Random
    depth match {
      case 0 => Empty
      case k => Node(from+r.nextInt(to-from), createTree(k-1, from, to), createTree(k-1, from, to))
    }
  }

  //ZADANIE 2 (3pkt)
  def preOrder[A](tree: BT[A]): List[A] =
    tree match {
      case Empty => Nil
      case Node(value, leftNode, rightNode) => value :: preOrder(leftNode) ::: preOrder(rightNode)
    }

  def toTree[A](xs: List[A]): BT[A] =
    xs match {
      case Nil => Empty
      //case
    }

  def test: List[Int] =
    preOrder(tst)

  /*def subtract(tree1: BT[Int], tree2: BT[Int]): BT[Int] = {
    def subtractLists(xs1: List[Int], xs2: List[Int]): List[Int] =
      xs1 match {
        case Nil => Nil
        case h :: t => h-xs2.head :: subtractLists(t, xs2.tail)
      }
  }*/

  val tst = Node(0, Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty)), Node(4, Node(5, Empty, Empty), Node(6, Empty, Empty)))

  def treeToString[A](tree: BT[A]): String = {
    tree match {
      case Empty => "\n"
      case Node(value, left, right) => treeToString(left) + " " + value + " " + treeToString(right)
    }
  }

  def a: String =
    inOrder(tst)

  def inOrder[A](tree: BT[A]): String =
    tree match {
      case Empty => ""
      case Node(value, leftNode, rightNode) => inOrder(leftNode) + " " + value + " " + inOrder(rightNode)
    }


  //ZADANIE 4 (5 pkt)
  def eachNElement[A](xs: LazyList[A], n: Int, howManyElements: Int): LazyList[A] = {
    if (n < 1 || howManyElements < 0) throw new Exception("Invalid arguments")
    def helper(xs: LazyList[A], elCounter: Int): LazyList[A] = {
      xs match {
        case h #:: t => if (elCounter == howManyElements) LazyList()
                        else if (elCounter % n == 0) h #:: helper(t, elCounter+1)
                        else helper(t, elCounter+1)
        case _ => LazyList()
      }
    }
      helper(xs, 0)
  }

  //ZADANIE 5 (5pkt)
  //Gdy wejściowe listy mają różną liczbę elementów: przy + oraz - brakujący element uznaje się za 0
  //Przy * brakujący element uznaje się za 1
  //Przy dzieleniu: x/Nil = x oraz Nil/x = 0
  def ldzialanie(xs1: LazyList[Double], xs2: LazyList[Double], operator: Char): LazyList[Double] =
    (operator, xs1, xs2) match {
      case ('+'|'-'|'*'|'/', LazyList(), LazyList()) => LazyList()
      case ('+'|'-'|'*'|'/', list, LazyList()) => list
      case ('+'|'*', LazyList(), list) => list
      case ('-', LazyList(), h #:: t) => -h #:: ldzialanie(LazyList(), t, '-')
      case ('/', LazyList(), _ #:: t) => 0 #:: ldzialanie(LazyList(), t, '/')
      case ('+', h1 #:: t1, h2 #:: t2) => (h1 + h2) #:: ldzialanie(t1, t2, '+')
      case ('-', h1 #:: t1, h2 #:: t2) => (h1 - h2) #:: ldzialanie(t1, t2, '-')
      case ('*', h1 #:: t1, h2 #:: t2) => (h1 * h2) #:: ldzialanie(t1, t2, '*')
      case ('/', h1 #:: t1, h2 #:: t2) => (h1 / h2) #:: ldzialanie(t1, t2, '/')
      case _ => throw new Exception("Undefined arithmetic operation")
    }

  /*def aaa[A](tree: BT[A]): String = {
    var s: String = "a"
    def helper(node: BT[A], space: Int): String = {
      node match {
        case Empty => s + ""
        case Node(value, leftNode, rightNode) =>
          s + helper(rightNode, space + 10) + "\n"
          for (i <- 10 until space) {s + " "}
          + value + "\n"
          helper(leftNode, space)
      }
      s
    }
    helper(tree, 0)
    s
  }*/

  /*def print2DUtil(root: Nothing, space: Int): Unit = { // Base case
    if (root == null) return
    // Increase distance between levels
    space += COUNT
    // Process right child first
    print2DUtil(root.right, space)
    // Print current node after space
    // count
    System.out.print("\n")
    for (i <- COUNT until space) {
      System.out.print(" ")
    }
    System.out.print(root.data + "\n")
    // Process left child
    print2DUtil(root.left, space)
  }*/

  // Wrapper over print2DUtil()
  //def print2D(root: Nothing): Unit = { // Pass initial space count as 0
   // print2DUtil(root, 0)
  //}


  /*def printTree[A](tree: BT[A]): Unit = {
    var space = 10
    def printRec(tree: BT[A], spac: Int) = {
      space += 10
      tree match {
        case Node(value, leftNode, rightNode) =>
      }
    }
  }*/

  /*def eachNElement[A](lxs: LazyList[A], n: Int, m: Int): LazyList[A] = {
    def helper(lxs: LazyList[A], n: Int, m: Int, currentList: LazyList[A]): LazyList[A] =
      (n, m) match {
        case (n, m) =>
      }
  }*/

}
