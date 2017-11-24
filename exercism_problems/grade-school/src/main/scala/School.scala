import scala.language.postfixOps

class School {
  type DB    = Map[Int, Seq[String]]
  type DBSEQ = (Int, Seq[String])

  var db: DB = Map()

  implicit val dbOrdering: Ordering[DBSEQ] = new Ordering[DBSEQ] {
    override def compare(x: DBSEQ, y: DBSEQ): Int = {
      (x _1) compare (y _1)
    }
  }

  private[this] def sortStudents(x: DBSEQ): DBSEQ = {
    (x._1, x._2 sorted)
  }

  private[this] def getWithUpdate(isFound: Option[Seq[String]],
                                  name: String): Seq[String] =
    (isFound getOrElse (Seq())) :+ name

  def add(name: String, g: Int): Unit =
    db += (g -> getWithUpdate(db get g, name))

  def grade(g: Int): Seq[String] =
    db getOrElse (g, Seq())

  def sorted:DB = {
    db = ((db toSeq) map (sortStudents) sorted).toMap
    db
  }

}
