package demo.worksheet

import scalaz._
import Scalaz._
object compute {
 def liftToStrong(name: String) = name.toUpperCase+"!"
                                                  //> liftToStrong: (name: String)String
 List("china","usa","japan").map(liftToStrong).map(print)
                                                  //> CHINA!USA!JAPAN!res0: List[Unit] = List((), (), ())
 case class Record(id: Int, content: String)
 case class Cache[A](data: A)
 implicit object cacheFunctor extends Functor[Cache] {
   def map[A,B](ca: Cache[A])(f: A => B): Cache[B] = Cache(f(ca.data))
 }
 val data = Cache[Record](Record(1,"I'm cached data"))
                                                  //> data  : demo.worksheet.compute.Cache[demo.worksheet.compute.Record] = Cache(
                                                  //| Record(1,I'm cached data))
 def markRecord(r: Record) = Record(r.id + 1000, r.content + " updated!")
                                                  //> markRecord: (r: demo.worksheet.compute.Record)demo.worksheet.compute.Record
 def saveToDB(r: Record) = println("saving record "+r.id)
                                                  //> saveToDB: (r: demo.worksheet.compute.Record)Unit
 data.map(markRecord).map(saveToDB)               //> saving record 1001
                                                  //| res1: demo.worksheet.compute.Cache[Unit] = Cache(())
 val listOfcache = List(Cache(Record(1,"rec1")),Cache(Record(2,"rec2")))
                                                  //> listOfcache  : List[demo.worksheet.compute.Cache[demo.worksheet.compute.Reco
                                                  //| rd]] = List(Cache(Record(1,rec1)), Cache(Record(2,rec2)))
 val listCacheFunctor = Functor[List] compose Functor[Cache]
                                                  //> listCacheFunctor  : scalaz.Functor[[α]List[demo.worksheet.compute.Cache[α]
                                                  //| ]] = scalaz.Functor$$anon$1@4a22f9e2
 val mdata = listCacheFunctor.map(
    listCacheFunctor.map(listOfcache)(markRecord))(saveToDB)
                                                  //> saving record 1001
                                                  //| saving record 1002
                                                  //| mdata  : List[demo.worksheet.compute.Cache[Unit]] = List(Cache(()), Cache(()
                                                  //| ))

System.getenv.containsKey("PATH")                 //> res2: Boolean = true
System.getenv.get("")                             //> res3: String = null
import java.sql.DriverManager
//val d = DriverManager.getConnection

val getsLen: String => Int = s => s.length        //> getsLen  : String => Int = <function1>
getsLen("abcd")                                   //> res4: Int = 4
val liftedLen = getsLen.point[List]               //> liftedLen  : List[String => Int] = List(<function1>)
Apply[List].ap(List("abcd"))(liftedLen)           //> res5: List[Int] = List(4)

trait Config[A] { def get: A }
object Config {
  def apply[A](a: A) = new Config[A] { def get = a }
  implicit val configFunctor = new Functor[Config] {
    def map[A,B](ca: Config[A])(f: A => B) = Config(f(ca.get))
  }
  implicit  val confApplicative = new Applicative[Config] {
      def point[A](a: => A) = Config(a)
      def ap[A,B](ca: => Config[A])(cfab: => Config[A => B]) =
        cfab map (_(ca.get))
   }
  
  implicit val confMonad = new Monad[Config] {
    def point[A](a: => A) = Config(a)
    def bind[A,B](ca: Config[A])(f: A => Config[B]) = f(ca.get)
  }
  
}
def map_[A,B](ca: Config[A])(f: A => B): Config[B] =
  Apply[Config].ap(ca)(f.point[Config])           //> map_ : [A, B](ca: demo.worksheet.compute.Config[A])(f: A => B)demo.workshee
                                                  //| t.compute.Config[B]
def incr(i: Int): Int = i + 1                     //> incr: (i: Int)Int
val fincr = incr _                                //> fincr  : Int => Int = <function1>
Apply[Config].ap(Config(3))((incr _).point[Config]).get
                                                  //> res6: Int = 4
^(Config(1),Config(2)){_ + _}.get                 //> res7: Int = 3
^^(Config(1),Config(2),Config(3)){_ + _ + _}.get  //> res8: Int = 6
(Config(1) |@| Config(2) |@| Config(3)){_ + _ + _}.get
                                                  //> res9: Int = 6
(Config("hello") |@| Config(" ") |@| Config("world")) {_ + _ + _}.get
                                                  //> res10: String = hello world
def greeting(hi: String, sp: String, w: String) = hi + sp + w
                                                  //> greeting: (hi: String, sp: String, w: String)String
val configGreet = Apply[Config].lift3(greeting _) //> configGreet  : (demo.worksheet.compute.Config[String], demo.worksheet.compu
                                                  //| te.Config[String], demo.worksheet.compute.Config[String]) => demo.worksheet
                                                  //| .compute.Config[String] = <function3>
configGreet(Config("hello"),Config(" "), Config("world!")).get
                                                  //> res11: String = hello world!
case class WebLogForm(usr: String, id: String, pwd: String)
def getUsr: Config[String] = Config("usr")        //> getUsr: => demo.worksheet.compute.Config[String]
def getId: Config[String] = Config("id")          //> getId: => demo.worksheet.compute.Config[String]
def getPwd: Config[String] = Config("pwd")        //> getPwd: => demo.worksheet.compute.Config[String]
^^(getUsr,getId,getPwd)(WebLogForm(_,_,_))        //> res12: demo.worksheet.compute.Config[demo.worksheet.compute.WebLogForm] = d
                                                  //| emo.worksheet.compute$$anonfun$main$1$Config$3$$anon$4@359f7cdf
(getUsr |@| getId |@| getPwd)((a,b,c) => WebLogForm(a,b,c))
                                                  //> res13: demo.worksheet.compute.Config[demo.worksheet.compute.WebLogForm] = d
                                                  //| emo.worksheet.compute$$anonfun$main$1$Config$3$$anon$4@1fa268de
import java.sql.DriverManager
//val jgetConnection = java.sql.DriverManager.getConnection("src","usr","pwd")
val sqlConnection = Apply[Config].lift3(java.sql.DriverManager.getConnection)
                                                  //> sqlConnection  : (demo.worksheet.compute.Config[String], demo.worksheet.com
                                                  //| pute.Config[String], demo.worksheet.compute.Config[String]) => demo.workshe
                                                  //| et.compute.Config[java.sql.Connection] = <function3>
//sqlConnection(Config("Source"),Config("User"),Config("Password"))
List(List(1,2),List(10,20)).flatMap(x => x.map(a => a))
                                                  //> res14: List[Int] = List(1, 2, 10, 20)
(Config("hello") >>= { hi => Config(" ") >>= {
  sp => Config("World").map { world => hi + sp + world }}}).get
                                                  //> res15: String = hello World
(Config(3) >>= (a => Config(2).map (b => a + b))) get
                                                  //> res16: Int = 5

(for {
  a <- Config(3)
  b <- Config(2)
  c <- Config(a+b)
} yield (c)).get                                  //> res17: Int = 5

 // Functor    :  map[A,B]    (F[A])(f:   A => B):  F[B]
 // Applicative:  ap[A,B]     (F[A])(f: F[A => B]): F[B]
 // Monad      :  flatMap[A,B](F[A])(f: A => F[B]): F[B]
 // Traverse   :  traverse[G:Applicative,A,B](F[A])(f: A => G[B]): G[F[B]]
 //               sequence[G:Applicative,A](F[G[A]]): G[F[A]]
 //   fa.flatMap(a => fb.flatMap(b => fc.flatMap(c => fd.map(...))))
 //   for {
 //      a <- (fa: F[A])
 //      b <- (fb: F[A])
 //      c <- (fc: F[A])
 //   } yield { ... }
 
 // a = e0
 // b = e1(a)
 // c = e2(a,b)
 // d = e1(c)
 //def e0: Id[Int] = 3
 //def e1(a: Int): Id[Int] = a + 2
 //def e2(a: Int, b: Int): Id[Int] = a + b
 def e0: Option[Int] = 3.some                     //> e0: => Option[Int]
 def e1(a: Int): Option[Int] = (a + 2).some       //> e1: (a: Int)Option[Int]
 def e2(a: Int, b: Int): Option[Int] = (a + b).some
                                                  //> e2: (a: Int, b: Int)Option[Int]
 def e3: Option[Int] = none[Int]                  //> e3: => Option[Int]
for {
  a <- e0
  b <- e1(a)
  z <- e3
  c <- e2(a,b)
 } yield c                                        //> res18: Option[Int] = None


 trait Book
 trait Author
 import concurrent._
 def books(author: Author): Future[Book] = ???    //> books: (author: demo.worksheet.compute.Author)scalaz.concurrent.Future[demo
                                                  //| .worksheet.compute.Book]
 def listFutureBooks(authors: List[Author]) = authors.map(books)
                                                  //> listFutureBooks: (authors: List[demo.worksheet.compute.Author])List[scalaz.
                                                  //| concurrent.Future[demo.worksheet.compute.Book]]
 def futureListBooks(authors: List[Author]) = authors.traverse(books)
                                                  //> futureListBooks: (authors: List[demo.worksheet.compute.Author])scalaz.concu
                                                  //| rrent.Future[List[demo.worksheet.compute.Book]]
 def futureListBooks_s(authors: List[Author]) = listFutureBooks(authors).sequence
                                                  //> futureListBooks_s: (authors: List[demo.worksheet.compute.Author])scalaz.con
                                                  //| current.Future[List[demo.worksheet.compute.Book]]

import scala.language.higherKinds
 sealed trait MyFree[F[_],A]
 object MyFree {
   final case class Return[F[_],A](a: A) extends MyFree[F,A]
   final case class Suspend[F[_],A](fa: F[A]) extends MyFree[F,A]
   final case class FlatMap[F[_],A,B](fa: MyFree[F,A], f: A => MyFree[F,B]) extends MyFree[F,B]
   
   def point[F[_],A](a: A) = Return[F,A](a)
   def bind[F[_],A,B](fa: MyFree[F,A])(f: A => MyFree[F,B]): MyFree[F,B] = FlatMap(fa,f)
   def map[F[_],A,B](fa: MyFree[F,A])(f: A => B): MyFree[F,B] = bind(fa)(f andThen (Return(_)))
 }
 
trait PrintMsg[A]
case class PrintLine(msg: String) extends PrintMsg[Unit]
val printHello: List[PrintLine] = List(PrintLine("Hello"),PrintLine(" world!"))
                                                  //> printHello  : List[demo.worksheet.compute.PrintLine] = List(PrintLine(Hello
                                                  //| ), PrintLine( world!))
printHello.map(a => print(a.msg))                 //> Hello world!res19: List[Unit] = List((), ())
}