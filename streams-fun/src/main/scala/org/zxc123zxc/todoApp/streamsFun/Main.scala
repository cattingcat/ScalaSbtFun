package org.zxc123zxc.todoApp.streamsFun

import cats.{Apply, Foldable, Monad}
import cats.instances.list._
import cats.implicits._
import cats.instances.option._
import cats.effect._
import fs2.io._
import cats.effect.Async

import scala.io.StdIn
import cats.effect.IO._
import sun.nio.cs.Surrogate

import scala.util.matching.Regex



trait Parsers[Err, Parser[+_]] {
  self =>

  def run[A](p: Parser[A])(input: String): Either[String, A]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(a => succeed(f(a)))
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(Nil)
  /** At least one result */
  def many1[A](p: Parser[A]): Parser[List[A]]
  def pslice[A](p: Parser[A]): Parser[String]
  def size[A](p: Parser[A]): Parser[Int] = map(pslice(p))(_.length)
  def product[A, B](pa: Parser[A], pb: => Parser[B]): Parser[(A, B)] = flatMap(pa)(a => flatMap(pb)(b => succeed((a, b))))
  def map2[A,B,C](pa: Parser[A], pb: => Parser[B])(f: (A,B) => C): Parser[C] = flatMap(pa)(a => flatMap(pb)(b => succeed(f(a,b))))
  def flatMap[A, B](pa: Parser[A])(f: A => Parser[B]): Parser[B]

  def succeed[A](a: A): Parser[A]
  implicit def char(c: Char): Parser[Char] = map(string(c.toString))(_.charAt(0))
  implicit def string(s: String): Parser[String]
  implicit def regex(r: Regex): Parser[String]

  implicit def idOps[A](p: Parser[A]): ParserOps[A] = ParserOps(p)
  implicit def ops[A](a: A)(implicit f: A => Parser[A]): ParserOps[A] = ParserOps(f(a))

  def whitespace: Parser[String] = "\\s*".r
  def skipL[A, B](pa: Parser[A], pb: Parser[B]): Parser[B] = flatMap(pa)(_ => flatMap(pb)(r => succeed(r)))
  def skipR[A, B](pa: Parser[A], pb: Parser[B]): Parser[A] = flatMap(pa)(r => flatMap(pb)(_ => succeed(r)))
  def optional[A](p: Parser[A]): Parser[Option[A]]

  case class ParserOps[A](p: Parser[A]) {
    def |(other: Parser[A]): Parser[A] = self.or(p, other)
    def or(other: Parser[A]): Parser[A] = self.or(p, other)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def pslice: Parser[String] = self.pslice(p)
    def **[B](pb: Parser[B]): Parser[(A, B)] = self.product(p, pb)
    def opt: Parser[Option[A]] = self.optional(p)
    def *>[B](pb: Parser[B]): Parser[B] = self.skipL(p, pb)
    def <*[B](pb: Parser[B]): Parser[A] = self.skipR(p, pb)
  }
}



//trait JSON
//object JSON {
//  case object JNull extends JSON
//  case class JNumber(get: Double) extends JSON
//  case class JString(get: String) extends JSON
//  case class JBool(get: Boolean) extends JSON
//  case class JArray(get: IndexedSeq[JSON]) extends JSON
//  case class JObject(get: Map[String, JSON]) extends JSON
//}
//
//object jsonParser {
//  def jsonParser[Err,Parser[+_]](P: Parsers[Err,Parser]): Parser[JSON] = {
//    import P._
//    val spaces = char(' ').many.slice
//    ???
//  }
//}

object Tmp {
  case class Location(offset: Int)

  case class ParserState(loc: Location, input: String)

  trait Result[+A] {
    def extract: Either[String, A] = this match {
      case Succeed(r, _) => Right(r)
      case Failure(err) => Left(err)
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Succeed(r, l) => Succeed(r, l + n)
      case f @ Failure(_) => f
    }
  }

  case class Succeed[+A](res: A, len: Int) extends Result[A]

  case class Failure(err: String) extends Result[Nothing]



  type Err = AnyRef
  type Parser[+A] = ParserState => Result[A]

  object SimpleParsers extends Parsers[Err, Parser] {
    override def run[A](p: Parser[A])(input: String): Either[String, A] = {
      val loc = Location(0)
      val state = ParserState(loc, input)
      val res = p(state)
      res.extract
    }

    override def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = s => {
      val p1Res = p1(s)

      p1Res match {
        case r@Succeed(_, _) => r
        case Failure(_) => p2(s)
      }
    }

    /** At least one result */
    override def many1[A](p: Parser[A]): Parser[List[A]] = ???

    override def pslice[A](p: Parser[A]): Parser[String] = s => {
      val r = p(s)

      r match {
        case Succeed(_, l) =>
          val res = s.input.substring(s.loc.offset, s.loc.offset + l)
          Succeed[String](res, l)
        case f@Failure(_) => f
      }
    }

    override def flatMap[A, B](pa: Parser[A])(f: A => Parser[B]): Parser[B] = s => {
      val res = pa(s)
      res match {
        case Succeed(r, l) =>
          val prevOffset = s.loc.offset
          val loc = s.loc.copy(offset = prevOffset + l)
          val ss = s.copy(loc = loc)

          val newR = f(r)(ss)

          newR.advanceSuccess(l) // We need to add offset from previous result

        case fail @ Failure(_) => fail
      }
    }

    override implicit def string(w: String): Parser[String] = s => {
      val in = s.input
      val ss = in.substring(s.loc.offset)
      if (ss.indexOf(w) == 0) {
        Succeed(w, w.length)
      } else {
        Failure(s"'$w' not found in '$in'")
      }
    }

    override implicit def regex(r: Regex): Parser[String] = s => {
      val ss = s.input.substring(s.loc.offset)
      r.findPrefixOf(ss) match {
        case None => Failure(s"Regex $r not found")
        case Some(m) => Succeed(m, m.length)
      }
    }

    override def optional[A](p: Parser[A]): Parser[Option[A]] = s => {
      val res = p(s)
      res match {
        case Failure(_) => Succeed(None, 0)
        case Succeed(r, l) => Succeed(Some(r), l)
      }
    }

    override def succeed[A](a: A): Parser[A] = _ => Succeed(a, 0)


    def surround[A, B, C](start: Parser[A], stop: Parser[B])(p: => Parser[C]): Parser[C] = start *> p <* stop
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello world")

    import Tmp.SimpleParsers._

    val sp = whitespace.opt
    val parser = surround(sp, sp)("bbb")
    val res = run(parser)("         bbb        ")

    println(res)











    val json ="""
{
  "Company name" : "Microsoft Corporation",
  "Ticker" : "MSFT",
  "Active" : true,
  "Price" : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}"""
  }
}
