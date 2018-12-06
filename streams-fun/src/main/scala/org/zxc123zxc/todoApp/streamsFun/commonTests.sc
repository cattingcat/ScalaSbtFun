

trait Foo {
  type Bar
  def foo: Bar
}

trait NewFoo[T] {
  def foo: T
}

val o = new Foo {
  override type Bar = String
  override def foo: Bar = ""
}

val a = new NewFoo[Int] {
  override def foo: Int = 33
}


val testFoo: (Int => List[String]) = null