package com.arulselvan.chapter7
import scalaz._

object Coyoneda {
  sealed abstract class Coyoneda[S[_], A] {
    def run(implicit S: Functor[S]): S[A]      = ???
    def trans[G[_]](f: S ~> G): Coyoneda[G, A] = ???
  }

  object Coyoneda {
    implicit def functor[S[_], A]: Functor[Coyoneda[S, ?]] = ???
    final case class Map[F[_], A, B](fa: F[A], f: A => B) extends Coyoneda[F, A]
    def apply[S[_], A, B](sa: S[A])(f: A => B) = Map[S, A, B](sa, f)
    def lift[S[_], A](sa: S[A])                = Map[S, A, A](sa, identity)
  }

  sealed abstract class ContravariantCoyoneda[S[_], A] {
    def run(implicit S: Contravariant[S]): S[A]             = ???
    def trans[G[_]](f: S ~> G): ContravariantCoyoneda[G, A] = ???
  }

  object ContravariantCoyoneda {
    implicit def contravariant[S[_], A]: Contravariant[ContravariantCoyoneda[S, ?]] = ???

    final case class ContraMap[F[_], A, B](fa: F[A], f: B => A) extends ContravariantCoyoneda[F, A]
    def apply[F[_], A, B](sa: F[A])(f: B => A) = ContraMap(sa, f)
    def lift[S[_], A](sa: S[A]): ContravariantCoyoneda[S, A] =
      ContraMap[S, A, A](sa, identity)
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
    def liftCoyo[F[_]](implicit I: Ast :<: F) = new Machines[Coyoneda[F, ?]] {
      def getTime                  = Coyoneda.lift(I.inj(GetTime()))
      def getManaged               = Coyoneda.lift(I.inj(GetManaged()))
      def getAlive                 = Coyoneda.lift(I.inj(GetAlive()))
      def start(node: MachineNode) = Coyoneda.lift(I.inj(Start(node)))
      def stop(node: MachineNode)  = Coyoneda.lift(I.inj(Stop(node)))
    }
    def liftCoyoyo[F[_]](implicit I: Ast :<: F) = new Machines[ContravariantCoyoneda[F, ?]] {
      def getTime                  = ContravariantCoyoneda.lift(I.inj(GetTime()))
      def getManaged               = ContravariantCoyoneda.lift(I.inj(GetManaged()))
      def getAlive                 = ContravariantCoyoneda.lift(I.inj(GetAlive()))
      def start(node: MachineNode) = ContravariantCoyoneda.lift(I.inj(Start(node)))
      def stop(node: MachineNode)  = ContravariantCoyoneda.lift(I.inj(Stop(node)))
    }
  }
}
