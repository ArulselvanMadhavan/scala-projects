package com.chapter1
import PrintableInstances._;
import PrintableSyntax._;
import cats.Show;
import cats.syntax.show._;
import cats.Eq;
import cats.syntax.eq._;
import cats.instances.string._;
import cats.instances.int._;
import cats.instances.option._;

final case class Cat(name:String, age:Int, color:String)

object Cat{

  implicit val printableCat:Printable[Cat] = new Printable[Cat] {
    def format(value:Cat):String = {
      val name  = Printable.format(value.name)
      val age   = Printable.format(value.age)
      val color = Printable.format(value.color)
      s"${name} is a ${age} year-old ${color} cat."
    }
  }

  implicit val dateShow: Show[Cat] = new Show[Cat] {
    def show(cat:Cat):String =
      s"${cat.name}\t${cat.age}\t${cat.color}"
  }

  implicit val catEquality: Eq[Cat] = new Eq[Cat] {
    def eqv(a:Cat, b:Cat):Boolean =
      a.name === b.name && a.age === b.age && a.color === b.color
  }
}

object Main {
  def main(args:Array[String]):Unit = {
    val res0 = Cat("Arul", 27, "Brown")
    res0.print
    println(res0.show)
    val cat1 = Cat("garfield",33,"ob");
    val cat2 = Cat("heathcliff",33,"ob");
    val optionCat1:Option[Cat] = Option(cat1)
    val optionCat2:Option[Cat] = Option.empty[Cat]
    println(cat1 === cat2)
    println(optionCat1 === optionCat2)
    println(cat1 =!= cat2)
    println(optionCat1 =!= optionCat2)
  }
}
