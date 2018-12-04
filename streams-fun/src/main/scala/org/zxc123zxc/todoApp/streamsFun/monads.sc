import scala.collection.Map

val l1 = List(1,2,3)
val l2 = List(4,5,6)

l1.flatMap(i => l2.map(j => (i, j)))

trait Monad[F[_]] {
  def unit[A](a: => A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => unit(f(a)))

  def sequence[A](lfa: List[F[A]]): F[List[A]] = lfa match {
    case h :: t => flatMap(h)(a => flatMap(sequence(t))(l => unit(a :: l)))
    case _ => unit(Nil)
  }

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la.map(f))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = map(fa)(a => List.fill(n)(a))

  def filterM[A](la: List[A])(f: A => F[Boolean]): F[List[A]] = {
    val tmp: List[F[(A, Boolean)]] = la.map(a => flatMap(f(a))(b => unit((a, b))))
    val tmp2: F[List[(A, Boolean)]] = sequence(tmp)

    flatMap(tmp2)(l => {
      val res = l.flatMap({
        case (a, true) => List(a)
        case _ => Nil
      })

      unit(res)
    })
  }

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)

  def flatMap2[A, B](fa: F[A])(f: A => F[B]): F[B] = {
    val wrap: F[A] => F[F[A]] = a => unit[F[A]](a)
    val id: F[A] => F[A] = compose(wrap, (a: F[A]) => a)
    compose(id, f)(fa)
  }

  // 12.7   all monads is applicative functors
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    flatMap(fa)(a => flatMap(fb)(b => unit(f(a,b))))
  }
}

val optMonad = new Monad[Option] {
  override def unit[A](a: => A) = Some(a)

  override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
    case Some(a) => f(a)
    case None => None
  }
}


optMonad.sequence(List(Some(1), Some(2), Some(3)))
optMonad.sequence(List(Some(1), None, Some(3)))
optMonad.sequence(Nil)

optMonad.filterM(List(1,2,3,4,5,6))(a => Some(a % 2 == 0))


case class State[S, A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = {
    State(s => {
      val (ra, rs) = run(s)
      (f(ra), rs)
    })
  }

  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (ra, rs) = run(s)
      f(ra).run(rs)
    })
  }
}





case class Person(firstName: String, lastName: String, age: Int)
val somePerson = Person("Qwe", "Asd", 123)

case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    override def unit[A](a: => A): Reader[R, A] = Reader[R, A](_ => a)

    override def flatMap[A, B](fa: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = {
      Reader[R, B](r => {
        val a = fa.run(r)
        val rrb = f(a)

        rrb.run(r)
      })
    }
  }
}

object pr {
  val personReaderMon = Reader.readerMonad[Person]

  val lastNameReader = Reader[Person, String](p => p.lastName)
  val firstNameReader = Reader[Person, String](p => p.firstName)
  val jsonReader = personReaderMon.flatMap(firstNameReader)(fn => Reader[Person, String](p => s"{ first: $fn, last: ${p.lastName} }"))
  val fullNameFormatReader = personReaderMon.flatMap(firstNameReader)(a => Reader(p => s"full name: $a ${p.lastName}"))
  val readerSeq = personReaderMon.sequence(List(firstNameReader, lastNameReader, fullNameFormatReader, jsonReader, fullNameFormatReader))
}


pr.readerSeq.run(somePerson)




// Ex 12.2
trait Applicative[F[_]] {
  def unit[A](a: => A): F[A]
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val curry: A => B => C = f.curried
    val fabc: F[A => B => C] = unit(curry)
    val fbc: F[B => C] = apply(fabc)(fa)
    val fc: F[C] = apply(fbc)(fb)
    fc
  }

  // Ex: 12.3
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
    val curry: A => B => C => D = f.curried
    val fabcd: F[A => B => C => D] = unit(curry)
    val fbcd: F[B => C => D] = apply(fabcd)(fa)
    val fcd: F[C => D] = apply(fbcd)(fb)
    val fd: F[D] = apply(fcd)(fc)
    fd
  }

  def sequence[A](l: List[F[A]]): F[List[A]] = l match {
    case h :: t => map2(h, sequence(t: List[F[A]]))((a, b) => a :: b)
    case _ => unit(Nil)
  }

  // 12.12
  def sequenceMap[K, V](m: Map[K, F[V]]): F[Map[K, V]] = {
    def seqList[K1, V1](l: List[(K1, F[V1])]): F[List[(K1, V1)]] = l match {
      case k -> v :: rest =>
        val elem: F[(K1, V1)] = map2(unit(k), v)((_, _))
        val restMap: F[List[(K1, V1)]] = seqList(rest)

        map2(elem, restMap)(_ :: _)
      case _ => unit(Nil)
    }

    def toMap(l: List[(K, V)]): Map[K, V] = l.toMap

    apply(unit(toMap))(seqList(m.toList))
  }
}

// 12.4
val stream1 = Stream(1,2,3,4,5,6,7,8,9,0)
val stream2 = Stream(11,12,13,14,15,16,17,18,19,10)

val applicativeStream = new Applicative[Stream] {
  override def unit[A](a: => A) = Stream.continually[A](a)

  override def apply[A, B](fab: Stream[A => B])(fa: Stream[A]): Stream[B] = {
    fa.zip(fab).map { case (el, f) => f(el) }
  }
}

applicativeStream.sequence(List(stream1, stream2)).toList





// 12.5
def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
  override def unit[A](a: => A): Either[E, A] = Right(a)

  override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] = {
    fa match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }
  }
}

val eiMonad = eitherMonad[String]
eiMonad.flatMap(Right(somePerson))(p => Left(p.lastName))

// Applicative calls all in mapN methods  (validation, we need all errors)
// Monads can skip calls in flatMap  (chains)

// TODO: 12.8-11
// TODO: 12.13 nd traversable






















