import scala.collection.immutable.Map

class Multiset(private val _multiples: Map[String, Int]):
  def this (elements: List[String]) =
    this(elements.groupBy(identity).view.mapValues(_.size).toMap)

  def toList: List[String] =
    _multiples
      .map((k, v) => (for i <- 1 to v yield k).toList)
      .reduce(_ ::: _)

  def +(rhs: Multiset) = Multiset(
    _multiples ++
    rhs._multiples
      .map((k, v) => k -> _multiples.getOrElse(k, 0).max(v))
  )

  def *(rhs: Multiset) = Multiset(
    _multiples
      .map((k, v) => k -> rhs._multiples.getOrElse(k, 0).min(v))
      .filter(_._2 > 0)
  )

  def -(rhs: Multiset) = Multiset(
    _multiples
      .map((k, v) => k -> (v - rhs._multiples.getOrElse(k, 0)))
      .filter(_._2 > 0)
  )

end Multiset 

@main def lab2 =
  val m1 = Multiset(List("one", "two", "three", "three", "two", "four", "two"))
  println("m1: " + m1.toList)

  val m2 = Multiset(List("one", "one", "three", "two", "three"))
  println("m2: " + m2.toList)

  println("m1 + m2: " + (m1 + m2).toList)
  println("m1 * m2: " + (m1 * m2).toList)
  println("m1 - m2: " + (m1 - m2).toList)
  println("m2 - m1: " + (m2 - m1).toList)
