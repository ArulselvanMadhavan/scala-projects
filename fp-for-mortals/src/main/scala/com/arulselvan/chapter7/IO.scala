package com.arulselvan.chapter7
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

object IO {
  sealed abstract class IO[E, A]

  object IO{
    type Task[A] = IO[Throwable, A]
    // Eager evaluation of an existing value.
    def now[E, A](a: A): IO[E, A] = ???
    // Lazy evaluation of a pure calculation.
    def point[E, A](a: =>A):IO[E, A] = ???
    // Lazy evaluation of a side-effecting, yet Total, code block.
    def sync[E, A](effect: =>A):IO[E, A] = ???
    // Lazy evaluation of a side effecting code block that may fail.
    def syncThrowable[A](effect: =>A):IO[Throwable, A] = ???
    // Create a failed IO
    def fail[E, A](error: E): IO[E, A] = ???
    //Async sleep for a specific period of time.
    def sleep[E](duration: Duration):IO[E, Unit] = ???

    object Task{
      def apply[A](effect: =>A):Task[A] = IO.syncThrowable(effect)
      def now[A](effect:A):Task[A] = IO.now(effect)
      def fail[A](error: Throwable):Task[A] = IO.fail(error)
      def fromFuture[E, A](io: Task[Future[A]])(ec: ExecutionContext):Task[A] = ???
    }
  }

  val fa: IO.Task[Future[String]] = ???
  val ta: IO.Task[String] = IO.Task.fromFuture(fa)(ExecutionContext.global)

}
