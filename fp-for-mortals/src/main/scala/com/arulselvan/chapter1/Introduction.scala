package com.arulselvan
import scala.concurrent.Future
import scalaz._
import Scalaz._
import simulacrum._

  trait Terminal[C[_]] {
    def read: C[String]
    def write(t: String): C[Unit]
  }

object Introduction {

  type Id[X] = X

  object TerminalSync extends Terminal[Id] {
    def read: String           = ???
    def write(t: String): Unit = ???
  }

  object TerminalAsync extends Terminal[Future] {
    def read: Future[String]           = ???
    def write(t: String): Future[Unit] = ???
  }

  trait Execution[C[_]] {
    def chain[A, B](c: C[A])(f: A => C[B]): C[B]
    def create[B](b: B): C[B]
  }

  object Execution {
    implicit class Ops[A, C[_]](c: C[A]) {
      def flatMap[B](f: A => C[B])(implicit e: Execution[C]): C[B] =
        e.chain(c)(f)
      def map[B](f: A => B)(implicit e: Execution[C]): C[B] =
        e.chain(c)(f andThen e.create)
    }
  }

  def echo[C[_]](implicit t: Terminal[C], e: Execution[C]): C[String] = {
    import Execution._
    for {
      in <- t.read
      _  <- t.write(in)
    } yield in
  }

  final class IO[A](val interpret: () => A) {
    def map[B](f: A => B): IO[B]         = IO(f(interpret()))
    def flatMap[B](f: A => IO[B]): IO[B] = IO(f(interpret()).interpret())
  }
  object IO {
    def apply[A](a: => A): IO[A] = new IO(() => a)
  }

  implicit object TerminalIO extends Terminal[IO] {
    def read: IO[String]           = IO { io.StdIn.readLine }
    def write(t: String): IO[Unit] = IO { println(t) }
  }

  implicit object ExecutionIO extends Execution[IO] {
    def chain[A, B](c: IO[A])(f: A => IO[B]): IO[B] = {
      for {
        a <- c
        b <- f(a)
      } yield b
    }
    def create[B](b: B): IO[B] = {
      new IO(() => b)
    }
  }

  def main(args:Array[String]):Unit = {

    val delayed: IO[String] = echo[IO]
    delayed.interpret()
  }
  // Version 2
  //   import Execution._
  //   t.read.flatMap { in: String =>
  //     t.write(in).flatMap { _: Unit =>
  //       e.create(in)
  //     }
  //   }
  // }
  // Version 1
  // e.chain(t.read) { in: String =>
  //   e.chain(t.write(in)) { _: Unit =>
  //     e.create(in)
  //   }
  // }
}
