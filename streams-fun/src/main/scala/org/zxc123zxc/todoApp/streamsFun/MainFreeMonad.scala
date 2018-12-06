package org.zxc123zxc.todoApp.streamsFun

import scala.annotation.tailrec
import scala.language.reflectiveCalls

trait Monad[F[_]] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

sealed trait Free[F[_], A]
final case class Return[F[_], A](a: A) extends Free[F, A]
final case class Suspend[F[_], A](fa: F[A]) extends Free[F, A]
final case class FlatMap[F[_], A, B](a: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {
  implicit def freeMonad[F[_]]: Monad[({type f[a] = Free[F, a]})#f] = new Monad[({type f[a] = Free[F, a]})#f] {
    def unit[A](a: => A): Free[F, A] = Return[F, A](a)
    def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = FlatMap[F, A, B](fa, f)
  }


  type TailRec[A] = Free[Function0, A]

//  @tailrec
//  def runF[A, B, C](f: TailRec[A]): A = f match {
//    case Return(a) => a
//    case Suspend(fa) => fa()
//    case FlatMap(Return(b), ffa) =>
//      val a: TailRec[A] = ffa(b)
//      runF[A, B, C](a)
//    case FlatMap(Suspend(fb: (() => B)), ffa: (B => TailRec[A])) =>
//      val b: B = fb()
//      val fa: TailRec[A] = ffa(b)
//      runF[A, B, C](fa)
//    case FlatMap(FlatMap(fc: TailRec[C], ffb: (C => TailRec[B])), ffa: ( B => TailRec[A])) => runF(FlatMap(fc, c => FlatMap(ffb(c()), ffa)))
//  }
}

object MainFreeMonad {
  def main(args: Array[String]): Unit = {
    println("Hello world")
  }
}
