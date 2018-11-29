package org.zxc123zxc.todoApp.webApp

import java.util.{Calendar, Date}

trait MySumCat[T] {
  def combine(a: T, b: T): T
}

// make option[T] categorized as MySumCat if T is MySumCat
class OptionMySumCat[T](implicit e: MySumCat[T]) extends MySumCat[Option[T]] {
  def combine(a: Option[T], b: Option[T]): Option[T] = (a, b) match {
    case (Some(x), Some(y)) => Some(e.combine(x, y))
    case _ => None
  }
}

// Instances
object MySumCat {
  implicit val intMySumCat = new MySumCat[Int] {
    override def combine(a: Int, b: Int): Int = a + b
  }

  implicit val strMySumCat = new MySumCat[String] {
    override def combine(a: String, b: String): String = a + b
  }

  // dynamic implicit builder
  implicit def optionMuSumCar[T](implicit e: MySumCat[T]): MySumCat[Option[T]] = new OptionMySumCat[T]

  def instance[T](f: (T, T) => T): MySumCat[T] = new MySumCat[T] {
    override def combine(a: T, b: T): T = f(a, b)
  }

  implicit val dateMySumCat = instance[Date] { (a, b) => new Date(a.getTime + b.getTime) }

  // New syntax
  object syntax {
    // add method <+> to all T that accord to MySumCat
    implicit class MySumCatOpts[T](a: T)(implicit e: MySumCat[T]) {
      def <+>(b: T): T = e.combine(a, b)
    }
  }
}





object CatFunMain {
  import MySumCat._
  import MySumCat.syntax._


  def main(args: Array[String]): Unit = {
    val o1: Option[Int] = Some(5)
    val o2: Option[Int] = Some(12)
    val s = o1 <+> o2
    println(s)

    val so1: Option[String] = Some("qweqwe")
    val so2: Option[String] = Some("asdasd")
    val ss = so1 <+> so2
    println(ss)

    println(123 <+> 234234)

    println("qweqwe" <+> "rtyrty")

    val cal = Calendar.getInstance()
    cal.set(1992, 3, 23)
    val a = cal.getTime
    cal.set(1, 3, 23)
    val b = cal.getTime
    println(a <+> b)
  }
}
