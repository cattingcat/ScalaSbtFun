package org.zxc123zxc.todoApp.webApp

import java.util.Date
import java.util.concurrent.Executors

import cats.data._
import cats.effect._
import cats.effect.syntax._
import cats.implicits._
import cats.{Applicative, ContravariantMonoidal, Eval, Functor, Id, Invariant, Monad, MonadError, Semigroup, Semigroupal}
import cats.instances.list._
import cats.instances.function._
import cats.instances.option._
import cats.syntax.writer._
import cats.instances.int._
import cats.syntax.semigroup._
import cats.syntax.monad._
import cats.syntax.functor._
import cats.syntax.applicative._

import scala.concurrent.{ExecutionContext, Future}
import scala.io.StdIn
import scala.util.{Random, Try}


case class MyClass[T](value: T)




object opts {
  implicit val myClassSemigroupal = new Semigroupal[MyClass] {
    override def product[A, B](fa: MyClass[A], fb: MyClass[B]): MyClass[(A, B)] = MyClass[(A, B)]((fa.value, fb.value))
  }

  implicit val myClassInvariant = new Invariant[MyClass] {
    override def imap[A, B](fa: MyClass[A])(f: A => B)(g: B => A): MyClass[B] = {

      MyClass(f(fa.value))
    }
  }


  implicit val myKleisli = new Kleisli[List, String, Int](s => s.split(",").map(i => i.trim.toInt).toList)

}




object MainIo extends IOApp {
  val mainPool = ExecutionContext.global
  val blockingPool = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  def foo(s: String) = s"Prefix $s postfix"

  override def run(args: List[String]): IO[ExitCode] = {
    val io = IO { println("Welcome!") }

    for(
      _ <- io;
      a <- IO { StdIn.readLine() };
      //_ <- IO.shift(blockingPool);
      b = foo(a);
      _ <- IO { println(b) };
      r <- IO { ExitCode.Success }
    ) yield r
  }
}


object Main {
  import opts._

  def main(args: Array[String]): Unit = {
    println("Hello world")

    MainIo.main(args)
  }
}
