package com.arulselvan.chapter7
import scalaz._
import Scalaz._
import scalaz.ioeffect.IO

object FreeLunch {
  // Free Monad is the data structure representation of Monad.
  sealed abstract class Free[S[_], A] {
    def mapSuspension[T[_]](f: S ~> T): Free[T, A] = ???
    def foldMap[M[_]: Monad](f: S ~> M): M[A]      = ???
  }

  object Free {
    implicit def monad[S[_], A]: Monad[Free[S, ?]] = ???

    // Suspend represents a program yet to be interpreted.
    private final case class Suspend[S[_], A](a: S[A]) extends Free[S, A]
    // Return -> Pure
    private final case class Return[S[_], A](a: A) extends Free[S, A]
    // GoSub -> Bind
    private final case class Gosub[S[_], A0, B](
        a: Free[S, A0],
        f: A0 => Free[S, B]
    ) extends Free[S, B]

    def liftF[S[_], A](value: S[A]): Free[S, A] = Suspend(value)

  }

  type Epoch       = Int
  type MachineNode = String

  trait Machines[F[_]] {
    def getTime: F[Epoch]
    def getManaged: F[NonEmptyList[MachineNode]]
    def getAlive: F[Map[MachineNode, Epoch]]
    def start(node: MachineNode): F[Unit]
    def stop(node: MachineNode): F[Unit]
  }

  object Machines {
    // Parameterized over the return type.
    sealed abstract class Ast[A]
    final case class GetTime()                extends Ast[Epoch]
    final case class GetManaged()             extends Ast[NonEmptyList[MachineNode]]
    final case class GetAlive()               extends Ast[Map[MachineNode, Epoch]]
    final case class Start(node: MachineNode) extends Ast[Unit]
    final case class Stop(node: MachineNode)  extends Ast[Unit]

    def liftF = new Machines[Free[Ast, ?]] {
      def getTime                  = Free.liftF(GetTime())
      def getManaged               = Free.liftF(GetManaged())
      def getAlive                 = Free.liftF(GetAlive())
      def start(node: MachineNode) = Free.liftF(Start(node))
      def stop(node: MachineNode)  = Free.liftF(Stop(node))
    }

    // Ast is part of the F instruction set.
    def liftF[F[_]](implicit I: Ast :<: F) = new Machines[Free[F, ?]] {
      def getTime                  = Free.liftF(I.inj(GetTime()))
      def getManaged               = Free.liftF(I.inj(GetManaged()))
      def getAlive                 = Free.liftF(I.inj(GetAlive()))
      def start(node: MachineNode) = Free.liftF(I.inj(Start(node)))
      def stop(node: MachineNode)  = Free.liftF(I.inj(Stop(node)))
    }

    def program[F[_]: Monad](M: Machines[F]): F[Unit] = ???
    val interpreter: Machines.Ast ~> IO[Throwable, ?] = ???
    val app: IO[Throwable, Unit] =
      program[Free[Machines.Ast, ?]](Machines.liftF).foldMap(interpreter)
    def interpreter[F[_]](f: Machines[F]): Ast ~> F = Lambda[Ast ~> F] {
      case GetTime()    => f.getTime
      case GetManaged() => f.getManaged
      case GetAlive()   => f.getAlive
      case Start(node)  => f.start(node)
      case Stop(node)   => f.stop(node)
    }
  }

  trait Drone[F[_]] {
    def getBacklog: F[Int]
    def getAgents: F[Int]
  }

  object Drone {

    sealed abstract class Ast[A]
    final case class GetBacklog() extends Ast[Int]
    final case class GetAgents()  extends Ast[Int]

    def liftF = new Drone[Free[Ast, ?]] {
      def getBacklog = Free.liftF(GetBacklog())
      def getAgents  = Free.liftF(GetAgents())
    }

    def liftF[F[_]](implicit I: Ast :<: F) = new Drone[Free[F, ?]] {
      def getBacklog = Free.liftF(I.inj(GetBacklog()))
      def getAgents  = Free.liftF(I.inj(GetAgents()))
    }

    def interpreter[F[_]](f: Drone[F]): Ast ~> F = Lambda[Ast ~> F] {
      case GetBacklog() => f.getBacklog
      case GetAgents()  => f.getAgents
    }
  }

  // How to create an AST that's a mix of Machines and Drone Ast
  // Use Coproduct
  // final case class Coproduct[F[_], G[_], A](run: F[A] \/ G[A])
  // Context: Free[Coproduct[Machines.Ast, Drone.Ast, ?], ?]

  // type :<:[F[_], G[_]] = Inject[F, G]
  // sealed abstract class Inject[F[_], G[_]] {
  //   def inj[A](fa: F[A]): G[A]
  //   def prj[A](ga: G[A]): Option[F[A]]
  // }

  // object Inject {
  //   implicit def left[F[_], G[_]]: F :<: Coproduct[F, G, ?] = ???
  //   implicit def right[F[_], G[_]]: G :<: Coproduct[F, G, ?] = ???
  // }

  def program[F[_]: Monad](M: Machines[F], D: Drone[F]): F[Unit] = ???
  val MachinesIO: Machines[IO[Throwable, ?]]                     = ???
  val DroneIO: Drone[IO[Throwable, ?]]                           = ???
  val M: Machines.Ast ~> IO[Throwable, ?]                        = Machines.interpreter(MachinesIO)
  val D: Drone.Ast ~> IO[Throwable, ?]                           = Drone.interpreter(DroneIO)

  // Larger InstructionSet that can operate on both Machines and Drones.
  // object NaturalTransformation {
  //   def or[F[_], G[_], H[_]](fg: F ~> G, hg: H ~> G): Coproduct[F, H, ?] ~> G = ???
  // }

  type Ast[a] = Coproduct[Machines.Ast, Drone.Ast, a]
  val interpreter: Ast ~> IO[Throwable, ?] = NaturalTransformation.or(M, D)
  // val app: IO[Throwable, Unit] = program[Free[Ast, ?]](Machines.liftF, Drone.liftF).foldMap(interpreter)
}
