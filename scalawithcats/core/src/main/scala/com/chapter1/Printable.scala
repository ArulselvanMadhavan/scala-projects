package com.chapter1

trait Printable[A] {
  def format(value:A):String
}

object PrintableInstances {

  implicit val intPrintable = new Printable[Int] {
    def format(value:Int):String =
      value.toString
  }

  implicit val stringPrintable = new Printable[String] {
    def format(value:String):String =
      value
  }

}

object Printable {

  def format[A](value:A)(implicit p:Printable[A]):String = {
    p.format(value)
  }

  def print[A](value:A)(implicit p:Printable[A]):Unit = {
    println(format(value))
  }

}

object PrintableSyntax {
  implicit class PrintableOps[A](value:A) {
    def format(implicit v:Printable[A]):String =
      v.format(value)

    def print(implicit v:Printable[A]):Unit =
      println(v.format(value))
  }
}
