package funsets

object Main extends App {
  import FunSets._

  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)
  val s4 = singletonSet(4)
  val s5 = singletonSet(5)
  val s6 = singletonSet(6)

  val suOdd = union(s1, union(s3, s5))

  println(contains(singletonSet(1), 1))

  printSet(map(suOdd, x => x + 1))
}
