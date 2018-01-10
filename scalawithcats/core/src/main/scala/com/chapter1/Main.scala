package com.chapter1
import PrintableInstances._;
import PrintableSyntax._;

final case class Cat(name:String, age:Int, color:String)

object Cat{
  implicit val printableCat:Printable[Cat] = new Printable[Cat] {
    def format(value:Cat):String = {
      val name = Printable.format(value.name)
      val age = Printable.format(value.age)
      val color = Printable.format(value.color)
      s"${name} is a ${age} year-old ${color} cat."
    }
  }
}

object Main {
  def main(args:Array[String]):Unit = {
    val res0 = Cat("Arul", 27, "Brown")
    res0.print
  }
}
