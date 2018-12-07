import scala.annotation.tailrec
import scala.concurrent

object StreamTransducers {

  sealed trait Process[I, O] {
    def apply(s: Stream[I]): Stream[O] = this match {
      case Halt() => Stream()
      case Await(recv) => s match {
        case h #:: t => recv(Some(h))(t)
        case xs => recv(None)(xs)
      }
      case Emit(head, tail) => head #:: tail(s)
    }

    def repeat: Process[I, O] = {
      def go(p: Process[I, O]): Process[I, O] = p match {
        case Halt() => go(this)
        case Await(recv) => Await {
          case None => recv(None)
          case i => go(recv(i))
        }
        case Emit(h, t) => Emit(h, go(t))
      }

      go(this)
    }

    // 15.5
    def |>[O2](p2: Process[O, O2]): Process[I, O2] = (this, p2) match {
      case (Halt(), _) => Halt()
      case (_, Halt()) => Halt()
      case (first @ _, Emit(h2, t2)) => Emit(h2, first |> t2)
      case (Emit(h1, t1), Await(f2)) => t1 |> f2(Some(h1))
      case (Await(f1), second @ _) => Await(i => f1(i) |> second)
    }
  }
  case class Emit[I, O](head: O, tail: Process[I, O] = Halt[I, O]()) extends Process[I, O]
  case class Await[I, O](recv: Option[I] => Process[I, O]) extends Process[I, O]
  case class Halt[I, O]() extends Process[I, O]

  def liftOne[I, O](f: I => O): Process[I, O] = Await {
    case Some(i) => Emit(f(i))
    case None => Halt()
  }

  def lift[I, O](f: I => O): Process[I, O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): Process[I, I] = Await[I, I] {
    case Some(i) if p(i) => Emit(i)
    case _ => Halt()
  }.repeat

  def sum: Process[Double, Double] = {
    def go(acc: Double): Process[Double, Double] = Await {
      case Some(d) =>
        val a = d + acc
        Emit(a, go(a))
      case _ => Halt()
    }

    go(0.0)
  }

  // 15.1:

  def take[I](n: Int): Process[I, I] = {
    def go(c: Int): Process[I, I] = Await {
      case Some(d) if c > 0 => Emit(d, go(c - 1))
      case _ => Halt()
    }

    go(n)
  }

  def drop[I](n: Int): Process[I, I] = {
    def foo: Process[I, I] = Await {
      case Some(d) => Emit(d, foo)
      case _ => Halt()
    }
    def go(c: Int): Process[I, I] = Await {
      case Some(_) if c > 0 => go(c - 1)
      case Some(d) => Emit(d, foo)
      case _ => Halt()
    }

    go(n)
  }

  def takeWhile[I](p: I => Boolean): Process[I, I] = {
    def go: Process[I, I] = Await {
      case Some(d) if p(d) => Emit(d, go)
      case _ => Halt()
    }

    go
  }

  def dropWhile[I](p: I => Boolean): Process[I, I] = {
    def foo: Process[I, I] = Await {
      case Some(d) => Emit(d, foo)
      case _ => Halt()
    }
    def go: Process[I, I] = Await {
      case Some(d) if p(d) => go
      case Some(d) => Emit(d, foo)
      case _ => Halt()
    }

    go
  }
  // 15.2
  def count[I]: Process[I, Int] = {
    def go(n: Int): Process[I, Int] = Await {
      case Some(d) =>
        val i = n + 1
        Emit(n, go(i))
      case _ => Halt()
    }

    go(1)
  }

  // 15.3
  def mean: Process[Double, Double] = {
    def go(sum: Double, n: Int): Process[Double, Double] = Await  {
      case Some(d) =>
        val s = sum + d
        val i = n + 1
        val m = s / i
        Emit(m, go(s, i))
      case _ => Halt()
    }

    go(0.0, 0)
  }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): Process[I, O] = Await {
    case Some(i) => f(i, z) match {
      case (o, s2) => Emit(o, loop(s2)(f))
    }
    case _ => Halt()
  }

  // 15.4:

  def sumL: Process[Double, Double] = loop(0.0)((i, s) => (s + i, s + i))

  def countL[I]: Process[I, Int] = loop(0)((_, s) => (s + 1, s + 1))

  def twice[I]: Process[I, I] = Await {
    case Some(i) => Emit(i, Emit(i, twice))
    case _ => Halt()
  }
}

val s = Stream(1, 2, 3, 4, 5, 6, 1, 1, 2)
val s1 = Stream(3,3,3,3,3,3,3)

val p0 = StreamTransducers.lift[Int, String](i => (i + 6).toString).repeat
val p1 = StreamTransducers.filter[Int](_ % 2 == 0)
val p2 = StreamTransducers.sum
val p3 = StreamTransducers.take[Int](3)
val p4 = StreamTransducers.drop[Int](3)
val p5 = StreamTransducers.takeWhile[Int](_ < 5)
val p6 = StreamTransducers.dropWhile[Int](_ < 5)
val p7 = StreamTransducers.count[Int]
val p8 = StreamTransducers.mean
val p9 = StreamTransducers.sumL
val p10 = StreamTransducers.countL[Int]
val p11 = StreamTransducers.twice[Int]



p0(s).take(10).toList
p1(s).take(10).toList
p2(s.map(_.toDouble)).take(10).toList
p3(s).take(10).toList
p4(s).take(10).toList
p5(s).take(10).toList
p6(s).take(10).toList
p7(s).take(10).toList
p8(s.map(_.toDouble)).take(10).toList
p8(s1.map(_.toDouble)).take(10).toList
p9(s.map(_.toDouble)).take(10).toList
p10(s).take(10).toList
p11(s).take(10).toList
(p1 |> p11 |> p10)(s).take(10).toList