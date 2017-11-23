import scala.language.postfixOps

object Bob {
  final val EXCLAIM  = "Whoa, chill out!"
  final val QUESTION = "Sure."
  final val SILENCE  = "Fine. Be that way!"
  final val WHATEVER = "Whatever."

  def isQuestion(str: List[Char]): Boolean =
    (str filterNot (_.isWhitespace) lastOption) contains ('?')

  def isSilence(str: List[Char]): Boolean =
    str filterNot (_.isWhitespace) isEmpty

  def isExclaim(str: List[Char]): Boolean = {
    val letters: List[Char] = str filter (_.isLetter)
    (letters nonEmpty) && (letters forall (_.isUpper))
  }

  def computeStats(listOfChars: List[Char]): (Boolean, Boolean, Boolean) =
    (isSilence(listOfChars), isExclaim(listOfChars), isQuestion(listOfChars))

  def response(statement: String): String =
    computeStats(statement.toList) match {
      case (true, _, _) => SILENCE
      case (_, true, _) => EXCLAIM
      case (_, _, true) => QUESTION
      case (_, _, _)    => WHATEVER
    }
}
