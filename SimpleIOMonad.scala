package demo.app
import scalaz._
import scalaz._
trait MyIO[+A] {self =>
  def run: A
  def map[B](f: A => B): MyIO[B] =
    new MyIO[B] {
      def run = f(self.run)
    }
  def flatMap[B](f: A => MyIO[B]): MyIO[B] =
    new MyIO[B] {
      def run = f(self.run).run
    }
}
object MyIO {
  def apply[A](a: A) = new MyIO[A] { def run = a }
  implicit val ioMonad = new Monad[MyIO] {
    def point[A](a: => A) = new MyIO[A] { def run = a }
    def bind[A,B](ma: MyIO[A])(f: A => MyIO[B]): MyIO[B] =
      ma flatMap f
  }
}
object MyIOFunctions {
  def ask(prompt: String): MyIO[String] = MyIO { println(prompt); readLine }
  def tell(msg: String): MyIO[Unit] = MyIO { println(msg) }
}
object MyIOPrgs {
  import MyIOFunctions._
  val prg: MyIO[Unit] = for {
    first <- ask("What's your first name?")
    last <- ask("What's your last name?")
    _ <- tell(s"Hello $first $last!")
  } yield()

}

object SimpleIOMonad extends App {
  import MyIOPrgs._
  prg.run

}
