package com.arulselvan.experiments.tutorial

import cats.effect._
import java.io._
import cats.implicits._
import java.net._
import cats.effect.syntax.all._
import cats.effect.ExitCase._

object Tutorial extends IOApp {

  def transfer(in: FileInputStream, out: FileOutputStream): IO[Long] = ???

  def copy(origin: File, dest: File): IO[Long] = inputOutputStreams(origin, dest).use {
    case (in, out) =>
      transfer(in, out)
  }

  def inputStream(f: File): Resource[IO, FileInputStream] =
    Resource.make {
      IO(new FileInputStream(f))
    } { inStream =>
      IO(inStream.close()).handleErrorWith(_ => IO.unit)
    }
  def outputStream(f: File): Resource[IO, FileOutputStream] =
    Resource.make {
      IO(new FileOutputStream(f))
    } { outStream =>
      IO(outStream.close()).handleErrorWith(_ => IO.unit)
    }

  def inputOutputStreams(in: File, out: File): Resource[IO, (FileInputStream, FileOutputStream)] = {
    for {
      i <- inputStream(in)
      o <- outputStream(out)
    } yield (i, o)
  }

  def inputStreamAutoCloseable(f: File): Resource[IO, FileInputStream] =
    Resource.fromAutoCloseable(IO(new FileInputStream(f)))

  def echoProtocol[F[_]: Sync](clientSocket: Socket): F[Unit] = {
    def loop(reader: BufferedReader, writer: BufferedWriter): F[Unit] =
      for {
        line <- Sync[F].delay(reader.readLine()) //
        _ <- line match {
              case "" => Sync[F].unit
              case _ =>
                Sync[F].delay { writer.write(line); writer.newLine(); writer.flush() } >> loop(
                  reader,
                  writer)
            }
      } yield ()

    def reader(clientSocket: Socket): Resource[F, BufferedReader] =
      Resource.make {
        Sync[F].delay(
          new BufferedReader(new InputStreamReader(clientSocket.getInputStream()))
        )
      } { reader =>
        Sync[F].delay(reader.close()).handleErrorWith(_ => Sync[F].unit)
      }

    def writer(clientSocket: Socket): Resource[F, BufferedWriter] =
      Resource.make {
        Sync[F].delay(
          new BufferedWriter(new PrintWriter(clientSocket.getOutputStream()))
        )
      } { writer =>
        Sync[F].delay(writer.close()).handleErrorWith(_ => Sync[F].unit)
      }

    def readerWriter(clientSocket: Socket): Resource[F, (BufferedReader, BufferedWriter)] =
      for {
        reader <- reader(clientSocket)
        writer <- writer(clientSocket)
      } yield (reader, writer)

    readerWriter(clientSocket).use {
      case (reader, writer) => //use returns cancelable IO.
        loop(reader, writer)
    }
  }

  def serve[F[_]: Concurrent](serverSocket: ServerSocket): F[Unit] = {
    def close(socket: Socket): F[Unit] =
      Sync[F].delay(socket.close()).handleErrorWith(_ => Sync[F].unit)

    for {
      _ <- Sync[F]
            .delay(serverSocket.accept())
            .bracketCase { socket =>
              echoProtocol(socket).guarantee(close(socket)).start
            } { (socket, exit) =>
              exit match {
                case Completed           => Sync[F].unit
                case Error(_) | Canceled => close(socket)
              }
            }
      _ <- serve(serverSocket)
    } yield ()
  }

  def run(args: List[String]): IO[ExitCode] = {
    def close[F[_]: Sync](socket: ServerSocket): F[Unit] =
      Sync[F].delay(socket.close()).handleErrorWith(_ => Sync[F].unit)

    IO(new ServerSocket(args.headOption.map(_.toInt).getOrElse(5432)))
      .bracket { serverSocket =>
        serve[IO](serverSocket) >> IO.pure(ExitCode.Success)
      } { serverSocket =>
        close[IO](serverSocket) >> IO(println("Server finished"))
      }
  }
}
