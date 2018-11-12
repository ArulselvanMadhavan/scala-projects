package com.arulselvan.chapter7
import java.util.concurrent.atomic.AtomicReference
import scalaz._
import scalaz.ioeffect._
object Promise {
  final class Promise[E, A] private(ref: AtomicReference[State[E, A]]) {
    def complete[E2](e: A):IO[E2, Boolean] = ???
    def error[E2](e: E): IO[E2, Boolean] = ???
    def get: IO[E, A] = ???
    def interrupt[E2](t: Throwable): IO[E2, Boolean] = ???    
  }

  object Promise {
    def make[E, A]: IO[E, Promise[E, A]] = ???
  }
}
