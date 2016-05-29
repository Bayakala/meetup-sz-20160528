package rehearsal.app
import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz._
import Scalaz._

object FreeADTs {
  trait Dialog[A]
  case class Ask(prompt: String) extends Dialog[String]
  case class Tell(msg: String) extends Dialog[Unit]
  implicit def dialogToFree[A](da: Dialog[A]) = Free.liftF(da)
}
object FreeASTs {
  import FreeADTs._
  val prg: Free[Dialog,Unit] = for {
    x <- Ask("What's your first name?")
    _ <- Tell(s"Hi, $x")
    y <- Ask("What's your last name?")
    _ <- Tell(s"Hello $x $y!!!")
  } yield()

}
object FreeInterp {
  import FreeADTs._
  object DialogConsole extends (Dialog ~> Id) {
    def apply[A](da: Dialog[A]): Id[A] = da match {
      case Ask(p) => println(p); readLine
      case Tell(s) => println(s)

    }
  }
  type WF[A] = Map[String,String] => A
  type Tester[A] = WriterT[WF,List[String],A]
  implicit val testerMonad = WriterT.writerTMonad[WF,List[String]]
  def testerToWriter[A](f: Map[String,String] => (List[String],A)) = WriterT[WF,List[String],A](f)
  object DialogTester extends (Dialog ~> Tester) {
    def apply[A](da: Dialog[A]): Tester[A] = da match {
      case Ask(p) => testerToWriter {m => (List(),m(p))}
      case Tell(s) => testerToWriter {m => (List(s),())}
    }
  }

}

object SimpleFree extends App {
 import FreeASTs._
 import FreeInterp._

 //prg.foldMapRec(DialogConsole)
  prg.foldMapRec(DialogTester).run(
    Map("What's your first name?" -> "Johnny", "What's your last name?" -> "Foo")
  )._1.map(println)

}
