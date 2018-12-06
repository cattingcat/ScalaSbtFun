import cats.effect.IO

import scala.annotation.tailrec

object simpleIO {
  trait IO { self =>
    def run: Unit

    def ++(io: IO): IO = new IO {
      override def run: Unit = {
        self.run
        io.run
      }
    }
  }

  object IO {
    def empty: IO = new IO {
      override def run: Unit = ()
    }
  }


  def PrintMsg: IO = new IO {
    def run = println("Hello world")
  }


  (PrintMsg ++ IO.empty).run
}



object monadicIO {
  trait Monad[F[_]] {
    def unit[A](a: => A): F[A]
    def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
  }

  trait IO[A] { self =>
    def run: A
    def flatMap[B](f: A => IO[B]): IO[B] = f(self.run)
    def map[B](f: A => B): IO[B] = new IO[B] { override def run: B = f(self.run) }
  }

  object IO extends Monad[IO] {
    override def unit[A](a: => A): IO[A] = new IO[A] { override def run: A = a}
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)

    /** Allow ''' val io = IO { readLine } ''' syntax */
    def apply[A](a: => A): IO[A] = unit(a)
  }


  def WriteLine(msg: String) = IO { println(msg) }
  def ReadLine: IO[String] = IO { "6.66" }

  def converter: IO[Unit] = for(
    _ <- WriteLine("Put double");
    v <- ReadLine.map(_.toDouble);
    _ <- WriteLine(s"Res = ${v + 1.0}")
  ) yield ()
}

monadicIO.converter.run


object caseClassedIO {
  sealed trait IO[A] { self =>
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(self, f)
    def map[B](f: A => B): IO[B] = FlatMap(self, (a: A) => Return(f(a)))
  }

  final case class Return[A](a: A) extends IO[A]
  final case class Suspend[A](a: () => A) extends IO[A]
  final case class FlatMap[A, B](sub: IO[A], f: A => IO[B]) extends IO[B]

  @tailrec
  def run[A, B, C](io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()

    case FlatMap(Return(a: B), f: (B => IO[A])) => run(f(a))
    case FlatMap(Suspend(r: (() => B)), f: (B => IO[A])) => run(f(r()))
    case FlatMap(FlatMap(sub: IO[C], g: (C => IO[B])), f: (B => IO[A])) =>
      // x - F[B]
      // f - B => F[A]
      // sub - F[C]
      // g - C => F[B]
      // Fix stack overflow:
      // FlatMap(FlatMap(y, g), f)  => FlatMap(y, yv => FlatMap(g(yv), f))
      run(FlatMap(sub, (c: C) => FlatMap(g(c), f)))
  }

  // Actually it can be used as TailRec trait
}
