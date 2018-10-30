package com.arulselvan.chapter4
import scalaz._
import Scalaz._
import simulacrum._
import eu.timepit.refined
import refined.api.Refined
import refined.numeric.Positive
import refined.collection.NonEmpty
import refined.refineV
import refined.W
import refined.boolean.And
import refined.collection.MaxSize
import eu.timepit.refined.api.Validate
import java.util.regex.Pattern

object Data {
  type |:[L, R] = Either[L, R]
  type Accepted = String |: Long |: Boolean
  // Encoding constraints in datatypes.
  final case class NonEmptyList[A](head: A, tail: IList[A])

  final case class Person private (name: String, age: Int)
  object Person {
    def apply(name: String, age: Int): Either[String, Person] = {
      if (name.nonEmpty && age > 0) {
        Right(new Person(name, age))
      } else {
        Left(s"Bad Input")
      }
    }
  }

  // Refined data types.
  object RefinedTypes {
    type Name = NonEmpty And MaxSize[W.`10`.T]
    final case class Person(name: String Refined Name, age: Int Refined Positive)
    sealed abstract class UrlEncoded
    object UrlEncoded {
      private[this] val valid: Pattern =
        Pattern.compile("\\A(\\p{Alnum}++|[-.*_+=&]++|%\\p{XDigit}{2})*\\z")
      implicit def urlValidate: Validate.Plain[String, UrlEncoded] = Validate.fromPredicate(
        s => valid.matcher(s).find(),
        identity,
        new UrlEncoded() {}
      )
    }
  }
}
