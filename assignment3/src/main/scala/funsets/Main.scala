package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  def k(x:Int):Boolean = ((x - 3) * x < 0);
  val M = map(k, x => x+2);
  println(-1, M(-1));
  println(0, M(0));
  println(1, M(1));
  println(2, M(2));
  println(3, M(3));
  println(4, M(4));
  println(5, M(5));
}
