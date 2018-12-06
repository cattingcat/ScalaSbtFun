import scala.annotation.tailrec
import scala.language.reflectiveCalls

object FreeMonadFun {
  trait Monad[F[_]] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  sealed trait Free[F[_], A]
  final case class Return[F[_], A](a: A) extends Free[F, A]
  final case class Suspend[F[_], A](fa: F[A]) extends Free[F, A]
  final case class FlatMap[F[_], A, B](a: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

  // type TailRec[A] = Free[Function0, A]
  // type IO[A] = Free[IO, A]
  // type Async[A] = Free[Par, A]



  // 13.1
  implicit def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] = new Monad[({type f[a] = Free[F, a]})#f] {
    def unit[A](a: => A): Free[F, A] = Return[F, A](a)
    def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = FlatMap[F, A, B](fa, f)
  }

  // 13.2
  @tailrec
  def runF[A](f: Free[Function0, A]): A = f match {
    case Return(a) => a
    case Suspend(fa) => fa()
    case FlatMap(Return(b), ffa) => runF[A](ffa(b))
    case FlatMap(Suspend(fb), ffa) =>
      val b = fb()
      val fa = ffa(b)
      runF[A](fa)
    case FlatMap(FlatMap(fc, ffb), ffa) => runF[A](FlatMap(fc, c => FlatMap(ffb(c()), ffa)))
  }



  // 13.3
  @tailrec
  def step[F[_], A, B, C](f: Free[F, A])(implicit M: Monad[F]) : Free[F, A] = f match {
    case FlatMap(Return(b: B), ffa: (B => Free[F, A])) => step(ffa(b))
    case FlatMap(FlatMap(ffc: Free[F, C], ffb: (C => Free[F, B])), ffa: (B => Free[F, A])) => step(FlatMap(ffc, (c: C) => FlatMap(ffb(c), ffa)))
    case _ => f
  }

  def run[F[_], A, B, C](f: Free[F, A])(implicit M: Monad[F]): F[A] = step(f) match {
    case Return(a: A) => M.unit(a)
    case Suspend(fa: F[A]) => fa
    case FlatMap(Suspend(fb: (F[B])), ffa: (B => Free[F, A])) =>
      val fbi: F[B] = fb.asInstanceOf[F[B]]
      M.flatMap[B, A](fbi)(b => run(ffa(b)))
    case _ => sys.error("step should process this case")
  }


  implicit val listMonad = new Monad[List] {
    override def unit[A](a: => A) = List(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }

  val r = run(Return[List, Int](55))

  println(r)
}