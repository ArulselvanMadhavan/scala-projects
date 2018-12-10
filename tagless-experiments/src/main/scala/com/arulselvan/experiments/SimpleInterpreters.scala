package com.arulselvan.experiments.taglessfinal
// import cats.syntax.either._
// import cats.instances.either._

// Read: https://gist.github.com/OlivierBlanvillain/48bb5c66dbb0557da50465809564ee80
object SimpleInterpreters {

  // Initial Encoding
  sealed trait IExp
  final case class Add(l: IExp, r: IExp) extends IExp
  final case class Neg(e: IExp)          extends IExp
  final case class Lit(i: Int)           extends IExp

  val ie: IExp = Add(Lit(8), Neg(Add(Lit(1), Lit(2))))

  // Final Encoding
  trait Exp[T] {
    def lit(i: Int): T
    def add(l: T, r: T): T
    def neg(e: T): T
  }

  object Exp {
    // Interpreters.
    implicit val evalExp: Exp[Int] = new Exp[Int] {
      def lit(i: Int)         = i
      def add(l: Int, r: Int) = l + r
      def neg(n: Int)         = -n
    }

    // // Another interpreter.
    implicit val printExp: Exp[String] = new Exp[String] {
      def lit(i: Int): String               = i.toString
      def neg(t: String): String            = s"(-$t)"
      def add(l: String, r: String): String = s"($l + $r)"
    }
  }

  // Represent exp 8 - (1 + 2).
  // Polymorphic Representation
  def tf0[T](implicit e: Exp[T]): T = {
    e.add(e.lit(8), e.neg(e.add(e.lit(1), e.lit(2))))
  }

  object ExpSyntax {
    def lit[T](i: Int)(implicit e: Exp[T]): T     = e.lit(i)
    def add[T](l: T, r: T)(implicit e: Exp[T]): T = e.add(l, r)
    def neg[T](n: T)(implicit e: Exp[T]): T       = e.neg(n)
  }

  //Simplified Tagless Final expression
  import ExpSyntax._
  def tf1[T: Exp]: T = {
    add(lit(8), neg(add(lit(1), lit(2))))
  }

  println(s"Int interpreter - ${tf1[Int]}")
  println(s"String interpreter - ${tf1[String]}")

  // Extensibility
  // Extend with Multiplication
  trait Mult[T] {
    def mul(l: T, r: T): T
  }

  object Mult {
    implicit val mi: Mult[Int] = new Mult[Int] {
      def mul(l: Int, r: Int): Int = l * r
    }
    implicit val printMult: Mult[String] = new Mult[String] {
      def mul(l: String, r: String): String = s"$l * $r"
    }
  }

  object MultSyntax {
    def mul[T](l: T, r: T)(implicit m: Mult[T]): T = m.mul(l, r)
  }

  import MultSyntax._

  // 7 - 1 * 2
  def tfm1[T: Exp: Mult] = add(lit(7), neg(mul(lit(1), lit(2))))
  // 7 * (8 - (1 + 2))
  def tfm2[T: Exp: Mult] = mul(lit(7), neg(add(lit(1), lit(2))))

  // val result = tfm1[String]
  // println(s"Result - ${result}")

  // Deserialization
  sealed trait Tree
  final case class Leaf(s: String)                 extends Tree
  final case class Node(s: String, ts: List[Tree]) extends Tree

  object Tree {
    implicit val toTree: Exp[Tree] with Mult[Tree] = new Exp[Tree] with Mult[Tree] {
      def lit(i: Int): Tree           = Node("lit", List(Leaf(i.toString)))
      def neg(t: Tree): Tree          = Node("neg", List(t))
      def add(l: Tree, r: Tree): Tree = Node("add", List(l, r))
      def mul(l: Tree, r: Tree): Tree = Node("mul", List(l, r))
    }
  }

  val tf1Tree = tf1[Tree]

  // Deserialization Attempt #1
  type ErrMsg = String

  def readInt(s: String): Either[ErrMsg, Int] = {
    import scala.util.{Try, Success, Failure}
    Try(s.toInt) match {
      case Success(i)  => Right(i)
      case Failure(ex) => Left(ex.toString)
    }
  }

  // T needs to be specified at the calling site.
  def fromTree[T](t: Tree)(implicit e: Exp[T]): Either[ErrMsg, T] =
    t match {
      case Node("lit", List(Leaf(n))) => readInt(n).map(e.lit)
      case Node("neg", List(t))       => fromTree(t).map(e.neg)
      case Node("add", List(l, r)) =>
        for {
          ll <- fromTree(l)
          rr <- fromTree(r)
        } yield e.add(ll, rr)
      case _ => Left("Unknown node in Tree")
    }

  trait Wrapped {
    def value[T](implicit e: Exp[T]): T
  }

  // def fromTreeWrapper(t:Tree):Either[ErrMsg, Wrapped] =
  //   t match {
  //     case Node("lit", List(Leaf(n))) => readInt(n).map(nn => new Wrapped{
  //       def value[T](implicit e:Exp[T]):T = e.lit(nn)
  //     })
  //     case Node("neg", List(t)) => fromTreeWrapper(t).map(nn => new Wrapped{
  //       def value[T](implicit e:Exp[T]):T = e.neg(nn)
  //     })
  //     case Node("add", List(l, r)) => for {
  //       ll <- fromTreeWrapper(l)
  //       rr <- fromTreeWrapper(r)
  //     } yield new Wrapped{
  //       def value[T](implicit e:Exp[T]):T = e.add(ll, rr)
  //     }
  //     case _ => Left("Unknown node in Tree")
  //   }

  object Wrapped {
    implicit val wrappingExp: Exp[Wrapped] = new Exp[Wrapped] {
      def lit(i: Int) = new Wrapped {
        def value[T](implicit e: Exp[T]): T = e.lit(i)
      }
      def add(l: Wrapped, r: Wrapped) = new Wrapped {
        def value[T](implicit e: Exp[T]): T = e.add(l.value[T], r.value[T])
      }
      def neg(n: Wrapped) = new Wrapped {
        def value[T](implicit e: Exp[T]): T = e.neg(n.value[T])
      }
    }
  }

  val ftWrapped = fromTree[Wrapped](tf1Tree)
  ftWrapped match {
    case Left(err) => println(s"Some err ${err}")
    case Right(w) => {
      println(s"W - int ${w.value[Int]}")
      println(s"W - string ${w.value[String]}")
    }
  }

  // Adding extensibility to Deserializer
  def fromTreeExt[T](
    recur: => Tree => Either[ErrMsg, T]
  )(implicit e: Exp[T]): Tree => Either[ErrMsg, T] = {
    tree => tree match {
      case Node("lit", List(Leaf(n))) =>
        readInt(n).map(e.lit)
      case Node("neg", List(t)) =>
        recur(t).map(e.neg)
      case Node("add", List(l, r)) =>
        for {
          ll <- recur(l)
          rr <- recur(r)
        } yield e.add(ll, rr)
    }
  }

  // Fix point - A fixed point of a function f is a point in which f a = a
  // A function that takes a byname parameter as input and returns A as output.
  def fix[A](f : (=> A) => A): A = f(fix(f))

  def fromTree2[T : Exp](t: Tree):Either[ErrMsg, T] = fix(fromTreeExt[T] _)(t)

  def fromTreeExtWithMult[T](
    recur: => Tree => Either[ErrMsg, T]
  )(implicit e: Exp[T], m: Mult[T]): Tree => Either[ErrMsg, T] = {
    tree => tree match {
      case Node("mul", List(l, r)) =>
        for {
          ll <- recur(l)
          rr <- recur(r)
        } yield m.mul(ll, rr)
      case t =>
        fromTreeExt(recur).apply(t)
    }
  }

  def fromTree3[T : Exp : Mult](t: Tree): Either[ErrMsg, T] = fix(fromTreeExtWithMult[T] _)(t)

  // Example: Serialize math expression to Tree
  val tfm1ToTree = tfm1[Tree]
  // Deserialize tree expression to string
  val tfm1Deserialized = fromTree3[String](tfm1ToTree)
  assert(fromTree3[String](tfm1[Tree]) == Right(tfm1[String]))

  // Transformation
  sealed trait Ctx
  final case object PosCtx extends Ctx
  final case object NegCtx extends Ctx

  object Ctx {
    implicit def negDownExp[T](implicit e: Exp[T]):Exp[Ctx => T] = new Exp[Ctx => T]{
      def lit(i:Int):Exp[Ctx => T] = {
        case PosCtx => e.lit(i)
        case NegCtx => e.neg(e.lit(i))
      }

      def neg(x: Ctx => T):Exp[Ctx => T] = {
        case PosCtx => x(NegCtx)
        case NegCtx => x(PosCtx)
      }

      def add(l: Ctx => T, r: Ctx => T):Exp[Ctx => T] = c => e.add(l(c), r(c))
    }
  }

    def pushNeg[T](e: Ctx => T): T = e(PosCtx)
}
