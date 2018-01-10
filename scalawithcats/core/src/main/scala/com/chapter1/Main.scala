package com.chapter1

final case class Cat(name:String, age:Int, color:String)

object Cat{
  implicit val printableCat:Printable[Cat] = new Printable[Cat] {
    def format(value:Cat):String =
      s"${value.name} is a ${value.age} year-old ${value.color} cat."
  }
}

object Main {
  def main(args:Array[String]):Unit = {
    Printable.print(Cat("Arul", 27, "Brown"))
  }
}
