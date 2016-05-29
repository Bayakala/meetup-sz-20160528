package demo.app

import scalaz._
import Scalaz._
import effect._
import IO._
import Free._
import scala.language.higherKinds
import scala.language.implicitConversions

object IOMonadPrg {
  def div(dvdn: Int, dvsor: Int): IO[Int] =
    IO(dvdn / dvsor)
  val ioprg: IO[Int] = for {
    _ <- putLn("enter dividend:")
    dvdn <- readLn
    _ <- putLn("enter divisor:")
    dvsor <- readLn
    quot <- div(dvdn.toInt, dvsor.toInt)
    _ <- putLn(s"the result:$quot")
  } yield quot

 // Flow Control : Monad Transformer >>> IO[Option[Int]]
  //implicit def ioToOptionT[A](io: IO[A]): OptionT[IO,A] = io.liftM[OptionT]
  val optionIOprg: OptionT[IO,Int] = for {
    _ <- putLn("enter dividend:").liftM[OptionT]
    dvdn <- readLn.liftM[OptionT]
    _ <- putLn("enter divisor:").liftM[OptionT]
    dvsor <- readLn.liftM[OptionT]
    a <- if (dvsor.toInt == 0 ) OptionT(IO(None: Option[String])) else IO(0).liftM[OptionT]
    quot <- div(dvdn.toInt, dvsor.toInt).liftM[OptionT]
    _ <- putLn(s"the result:$quot").liftM[OptionT]
  } yield quot

// Error handling and Logging, IO[Writer[List[String],Int]]
  type WriterTIO[F[_],A] = WriterT[F,List[String],A]
  val writerIOprg: WriterT[IO,List[String],Int] = for {
    _ <- putLn("enter dividend:").liftM[WriterTIO]
    dvdn <- readLn.liftM[WriterTIO]
    _ <- WriterT.writerT((List(s"received dividend $dvdn;"),dvdn).point[IO])
    _ <- putLn("enter divisor:").liftM[WriterTIO]
    dvsor <- readLn.liftM[WriterTIO]
    _ <- WriterT.writerT(IO(List(s"received divisor $dvsor, ready to divide ..."),dvdn))
    quot <- div(dvdn.toInt, dvsor.toInt).except(e => IO({println(e.getMessage());-99})).liftM[WriterTIO]
    _ <- if (quot < 0) WriterT.writerT((List(s"divide by zero Error!!!"),-99).point[IO]) else putLn(s"the result:$quot").liftM[WriterTIO]
  } yield (quot)


}

object IOMonad extends App {
  import IOMonadPrg._
   // ioprg.unsafePerformIO()
  //optionIOprg.run.unsafePerformIO()
  (writerIOprg.run.unsafePerformIO)._1.map(println)
}