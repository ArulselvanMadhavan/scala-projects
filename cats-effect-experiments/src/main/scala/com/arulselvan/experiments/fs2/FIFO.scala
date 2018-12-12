package com.arulselvan.experiments.fs2

import cats.implicits._
import cats.effect.{Concurrent, ExitCode, IO, IOApp}
import fs2.concurrent.Queue
import fs2.Stream
import scala.concurrent.duration._

class Buffering[F[_]](q1: Queue[F, Int], q2: Queue[F, Int])(implicit F: Concurrent[F]) {

  def start: Stream[F, Unit] =
    Stream(
      Stream.range(0, 1000).covary[F].to(q1.enqueue),
      q1.dequeue.to(q2.enqueue),
      q2.dequeue.evalMap(n => F.delay(println(s"Pulling out $n from Queue#2")))
    ).parJoin(3)
}

object FIFO extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val stream = for {
      q1 <- Stream.eval(Queue.bounded[IO, Int](1))
      q2 <- Stream.eval(Queue.bounded[IO, Int](100))
      bp = new Buffering[IO](q1, q2)
      _  <- Stream.sleep_[IO](5.seconds) concurrently bp.start.drain
    } yield ()
    stream.compile.drain.as(ExitCode.Success)
  }
}
