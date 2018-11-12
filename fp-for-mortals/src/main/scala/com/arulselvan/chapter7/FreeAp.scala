package com.arulselvan.chapter7

import scalaz._
import Scalaz._

object FreeAp {

  sealed abstract class FreeAp[S[_], A] {
    def hoist[G[_]](f: S ~> G): FreeAp[G, A]        = ???
    def foldMap[G[_]: Applicative](f: S ~> G): G[A] = ???
    // Generate Free[S, A] from FreeAp[S, A].
    def monadic: Free[S, A] = ???
    def analyze[M: Monoid](f: S ~> λ[α => M]): M =
      foldMap(λ[S ~> Const[M, ?]](x => Const(f(x)))).getConst
  }

  object FreeAp {
    implicit def applicative[S[_], A]: Applicative[FreeAp[S, ?]] = ???
    private final case class Pure[S[_], A](a: A) extends FreeAp[S, A]
    private final case class Ap[S[_], A, B](
        value: () => S[B],
        function: () => FreeAp[S, B => A]
    ) extends FreeAp[S, A]

    def pure[S[_], A](a: A): FreeAp[S, A]       = Pure(a)
    def lift[S[_], A](x: => S[A]): FreeAp[S, A] = ???
  }

  type Epoch       = Int
  type MachineNode = String
  type WorldView   = String

  trait Machines[F[_]] {
    def getTime: F[Epoch]
    def getManaged: F[NonEmptyList[MachineNode]]
    def getAlive: F[Map[MachineNode, Epoch]]
    def start(node: MachineNode): F[Unit]
    def stop(node: MachineNode): F[Unit]
  }

  object Machines {
    sealed abstract class Ast[A]
    final case class GetTime()                extends Ast[Epoch]
    final case class GetManaged()             extends Ast[NonEmptyList[MachineNode]]
    final case class GetAlive()               extends Ast[Map[MachineNode, Epoch]]
    final case class Start(node: MachineNode) extends Ast[Unit]
    final case class Stop(node: MachineNode)  extends Ast[Unit]
    def liftA[F[_]](implicit I: Ast :<: F) = new Machines[FreeAp[F, ?]] {
      def getTime                  = FreeAp.lift(I.inj(GetTime()))
      def getManaged               = FreeAp.lift(I.inj(GetManaged()))
      def getAlive                 = FreeAp.lift(I.inj(GetAlive()))
      def start(node: MachineNode) = FreeAp.lift(I.inj(Start(node)))
      def stop(node: MachineNode)  = FreeAp.lift(I.inj(Stop(node)))
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

    def liftA[F[_]](implicit I: Ast :<: F) = new Drone[FreeAp[F, ?]] {
      def getBacklog = FreeAp.lift(I.inj(GetBacklog()))
      def getAgents  = FreeAp.lift(I.inj(GetAgents()))
    }
  }

  trait Batch[F[_]] {
    def start(nodes: NonEmptyList[MachineNode]): F[Unit]
  }

  object Batch {
    sealed abstract class Ast[A]
    final case class Start(nodes: NonEmptyList[MachineNode]) extends Ast[Unit]
    def liftA[F[_]](implicit I: Ast :<: F) = new Batch[FreeAp[F, ?]] {
      def start(nodes: NonEmptyList[MachineNode]): FreeAp[F, Unit] =
        FreeAp.lift(I.inj(Start(nodes)))
    }
    type Orig[a] = Coproduct[Machines.Ast, Drone.Ast, a]

    sealed trait DynAgents[F[_]]

    final class DynAgentsModule[F[_]: Applicative](D: Drone[F], M: Machines[F])
        extends DynAgents[F] {
      def act(world: WorldView): F[WorldView] = ???
    }
    val world: WorldView                = ???
    val program                         = new DynAgentsModule(Drone.liftA[Orig], Machines.liftA[Orig])
    val freeap: FreeAp[Orig, WorldView] = program.act(world)
    val gather = λ[Orig ~> λ[α => IList[MachineNode]]] {
      case Coproduct(-\/(Machines.Start(node))) => IList.single(node)
      case _                                    => IList.empty
    }
    val gathered: IList[MachineNode] = freeap.analyze(gather)

    type Extended[a] = Coproduct[Batch.Ast, Orig, a]
    def batch(nodes: IList[MachineNode]): FreeAp[Extended, Unit] =
      nodes.toNel match {
        case None        => FreeAp.pure(())
        case Some(nodes) => FreeAp.lift(Coproduct.leftc(Batch.Start(nodes)))
      }

    val nostart = λ[Orig ~> FreeAp[Extended, ?]] {
      case Coproduct(-\/(Machines.Start(_))) => FreeAp.pure(())
      case other                             => FreeAp.lift(Coproduct.rightc(other))
    }

    val patched = batch(gathered) *> freeap.foldMap(nostart)

    def optimze[A](orig: FreeAp[Orig, A]): FreeAp[Extended, A] =
      (batch(orig.analyze(gather))) *> orig.foldMap(nostart)
  }
}
