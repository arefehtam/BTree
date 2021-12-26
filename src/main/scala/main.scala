import scala.collection.immutable.ListMap

object main extends App {
  var mapping: Map[Int, Int] = Map.empty[Int, Int] //  a dictionary with key as hline and value as depth
  var result: Map[Int, Any] = Map.empty[Int, Any] // a dictionary with key as hline and value as node name

  def traverse1(b: Option[BTree], hLine: Int = 0, depth: Int = 0): Unit =
    b match {
      case Some(BTree(n, l, r)) =>
        // check if there is any horizontal line with lower depth and equal hline then replace it with previous mapping
        if (mapping.getOrElse(hLine, depth) >= depth) {
          mapping += (hLine -> depth)
          result += (hLine -> n)
        }
        if (l.isDefined) {
          traverse1(l, hLine - 1, depth + 1)
        }
        if (r.isDefined) {
          traverse1(r, hLine + 1, depth + 1)
        }
      case None =>
    }

  var sortedList: List[Any] = List.empty[Any] // sorted nodes as a result
  var loc: Map[Any, Int] = Map.empty[Any, Int] // index of each node in sorted list
  mapping = Map.empty[Int, Int]
  def traverse2(b: Option[BTree], hLine: Int = 0, depth: Int = 0): Unit =
    b match {
      case Some(BTree(n, l, r)) =>
        // check if there is any horizontal line with lower depth and equal hline
        if (l.isDefined) {
          traverse2(l, hLine - 1, depth + 1)
        }
        if (mapping.getOrElse(hLine, depth) >= depth) {
          mapping += (hLine -> depth)
          if (loc.contains(n))
            sortedList = sortedList.updated(loc(n), n)
          else {
            sortedList ::= n
//            loc += (n -> sortedList.length - 1)
            loc = loc.updated(n, sortedList.length - 1)
          }
        }
        if (r.isDefined) {
          traverse2(r, hLine + 1, depth + 1)
        }
      case None =>
    }

  //  val bTree = Some(BTree("A", Some(BTree("B", None, None)), Some(BTree("C", Some(BTree("D", None, None)), None))))
  val bTree = Some(BTree("A", Some(BTree("B", None, None)), Some(BTree("C",
    Some(BTree("D", Some(BTree("E", Some(BTree("F", None, None)), None)), None)), None))))
  traverse1(bTree)

  print("Traverse1: ")
  println(ListMap(result.toSeq.sortBy(_._1): _*).values)

  traverse2(bTree)

  print("Traverse2: ")
  println(sortedList.reverse)

}
