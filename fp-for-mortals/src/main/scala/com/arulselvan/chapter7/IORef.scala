package com.arulselvan.chapter7
import java.util.concurrent.atomic.AtomicReference

// Atomic Mutable variable.
object IORef {
  final class IORef[A] private(ref: AtomicReference[A]) {
    def read[E]:IO[E, A] = ???
    // write with immediate consistency guarantees.
    def write[E](a: A): IO[E, Unit] = ???
    // Write with eventual consistency guarantees.
    def writeLater[E](a: A): IO[E, Unit] = ???
    // return true if an immediate write succeeded, if not return false (and abort)
    def tryWrite[E](a: A): IO[E, Boolean] = ???

    //
  }
}
