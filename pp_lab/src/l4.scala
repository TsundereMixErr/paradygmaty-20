import scala.annotation.tailrec

object l4 {
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

  //Funkcja pomocnicza wyswietlajaca drzewa
  def printBT[A](tree: BT[A]): Unit = {
    def innerPrintBT(tree: BT[A], depth: Int): Unit =
      tree match {
        case Node(value, leftNode, rightNode) =>
          innerPrintBT(rightNode, depth+1)
          for (i <- 0 until depth-1) print("...")
          print(value + "\n")
          innerPrintBT(leftNode, depth+1)
        case Empty =>
          for (i <- 0 until depth-1) print("...")
          print("||\n")
      }
    innerPrintBT(tree, 0)
  }

  //ZADANIE 1 (3pkt)
  def createTree(depth: Int, from: Int, to: Int): BT[Int] = {
    val r = scala.util.Random
    depth match {
      case 0 => Empty
      case k => Node(from+r.nextInt(to-from), createTree(k-1, from, to), createTree(k-1, from, to))
    }
  }

  //ZADANIE 2 (3pkt)
  def subtract(tree1: BT[Int], tree2: BT[Int]): BT[Int] =
    (tree1, tree2) match {
      case (Node(value1, leftNode1, rightNode1), Node(value2, leftNode2, rightNode2)) =>
        Node(value1 - value2, subtract(leftNode1, leftNode2), subtract(rightNode1, rightNode2))
      case _ => Empty
    }

  //ZADANIE 3 (4pkt)
  def removeDuplicatesDFS(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
    (tree1, tree2) match {
      case (Empty, Empty) => (Empty, Empty)
      case (Node(value1, leftNode1, rightNode1), Node(value2, leftNode2, rightNode2)) =>
        val leftSubtree = removeDuplicatesDFS(leftNode1, leftNode2)
        val rightSubtree = removeDuplicatesDFS(rightNode1, rightNode2)
        if(value1 == value2 && leftSubtree._1 == Empty && rightSubtree._1 == Empty) (Empty, Empty)
        else if(value1 == value2) (Node(-1, leftSubtree._1, rightSubtree._1), Node(-1, rightSubtree._2, rightSubtree._2))
        else (Node(value1, leftSubtree._1, rightSubtree._1), Node(value2, leftSubtree._2, rightSubtree._2))
    }
  }

  def removeDuplicatesBFS(tree1: BT[Int], tree2: BT[Int]): (BT[Int], BT[Int]) = {
    def innerRemoveDuplicatesBFS(tree1: BT[Int], tree2: BT[Int], queue: List[(BT[Int], BT[Int])],numberOfNode: Int, even: Int):
    (List[(Int, Int)], List[(Int, Int)]) =
      queue match {
        case Nil => (Nil, Nil)
        case (Empty,Empty) :: _ => (Nil, Nil)
        case (Node(value1, leftNode1, rightNode1), Node(value2, leftNode2, rightNode2)) :: tail =>
          if(value1 == value2 && areSubtreesEqual(tree1, tree2)) {
            if(tail == Nil) (Nil, Nil)
            else innerRemoveDuplicatesBFS(queue.tail.head._1, queue.tail.head._2, queue.tail,
              if(leftNode1 == Empty) numberOfNode
              else numberOfNode + 1,
              even + 1)
          }
          else if(value1 == value2) {
            val queue2 = queue.tail ::: List((leftNode1, leftNode2), (rightNode1, rightNode2))
            val result = innerRemoveDuplicatesBFS(queue2.tail.head._1, queue2.tail.head._2, queue2.tail,
            if(leftNode1 == Empty) numberOfNode
            else if(even % 2 == 0) 2 * numberOfNode
            else 2 * numberOfNode + 1,
              even + 1)
            ((-1, numberOfNode) :: result._1, (-1, numberOfNode) :: result._2)
          }
          else {
            val queue2 = queue ::: List((leftNode1, leftNode2), (rightNode1, rightNode2))
            val result = innerRemoveDuplicatesBFS(queue2.tail.head._1, queue2.tail.head._2, queue2.tail, numberOfNode + 1, even + 1)
            ((value1, numberOfNode) :: result._1, (value2, numberOfNode) :: result._2)
          }
      }
    val list = innerRemoveDuplicatesBFS(tree1, tree2, List((tree1, tree2)), 1, 0)
    (listToTree(1, countNodes(tree1), list._1), listToTree(1, countNodes(tree2), list._2))
  }

  //Funkcje pomocnicze:
  def areSubtreesEqual[A](tree1: BT[A], tree2: BT[A]): Boolean = {
    @tailrec
    def areSubtreesEqualRec(xs: List[(BT[A], BT[A])]): Boolean =
      xs match {
        case Nil => true
        case (Empty, Empty) :: _ => true
        case (Node(value1, leftNode1, rightNode1), Node(value2, leftNode2, rightNode2)) :: tail =>
          value1 == value2 && areSubtreesEqualRec(tail ::: List((leftNode1, leftNode2), (rightNode1, rightNode2)))
      }
    areSubtreesEqualRec(List((tree1, tree2)))
  }

  @tailrec
  def checkIfContains[A](xs: List[(A, A)], element: A, index: Int): Int =
    xs match {
      case Nil => -1
      case head :: tail => if(head._2 == element) index
                           else checkIfContains(tail, element, index + 1)
    }

  def getValueAt[A](xs: List[A], index: Int): A = {
    @tailrec
    def getValueAtRec(xs: List[A], index: Int, current: Int): A =
      current match {
        case index => xs.head
        case _ => getValueAtRec(xs.tail, index, current + 1)
      }
    getValueAtRec(xs, index, 0)
  }

  def listToTree(actualNode: Int, numberOfNodes: Int, listOfNodes: List[(Int,Int)]): BT[Int] = {
    if (actualNode > numberOfNodes || checkIfContains(listOfNodes, actualNode, 0) == -1) Empty
    else Node(getValueAt(listOfNodes, checkIfContains(listOfNodes, actualNode, 0))._1,
      listToTree(actualNode*2, numberOfNodes, listOfNodes), listToTree(actualNode*2+1, numberOfNodes, listOfNodes))
  }

  def countNodes(tree: BT[Int]): Int =
    tree match {
      case Empty => 0
      case Node(_, l, r) => 1 + countNodes(l) + countNodes(r)
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
  def ldzialanie2(xs1: LazyList[Double], xs2: LazyList[Double], operator: Char): LazyList[Double] =
    (operator, xs1, xs2) match {
      case ('+'|'-'|'*'|'/', LazyList(), LazyList()) => LazyList()
      case ('+'|'-'|'*'|'/', list, LazyList()) => list
      case ('+'|'*', LazyList(), list) => list
      case ('-', LazyList(), h #:: t) => -h #:: ldzialanie2(LazyList(), t, '-')
      case ('/', LazyList(), _ #:: t) => 0 #:: ldzialanie2(LazyList(), t, '/')
      case ('+', h1 #:: t1, h2 #:: t2) => (h1 + h2) #:: ldzialanie2(t1, t2, '+')
      case ('-', h1 #:: t1, h2 #:: t2) => (h1 - h2) #:: ldzialanie2(t1, t2, '-')
      case ('*', h1 #:: t1, h2 #:: t2) => (h1 * h2) #:: ldzialanie2(t1, t2, '*')
      case ('/', h1 #:: t1, h2 #:: t2) => (h1 / h2) #:: ldzialanie2(t1, t2, '/')
      case _ => throw new Exception("Undefined arithmetic operation")
    }

  //Wersja z funkcja zamiast chara jako operator
  def ldzialanie(xs1: LazyList[Double], xs2: LazyList[Double], operator: (Double, Double) => Double): LazyList[Double] =
    (xs1, xs2) match {
      case (LazyList(), _) => xs2
      case (_, LazyList()) => xs1
      case (h1 #:: t1, h2 #:: t2) => operator(h1, h2) #:: ldzialanie(t1, t2, operator)
    }

}
