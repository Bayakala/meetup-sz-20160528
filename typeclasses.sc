package demo.worksheet
import scala.language.implicitConversions
import scala.language.higherKinds
object typeclasses {
def sum0(xa: List[Int]): Int =
  xa.foldLeft(0){_ + _}                           //> sum0: (xa: List[Int])Int
sum0(List(1,2,3))                                 //> res0: Int = 6

object intAdder {
  val mzero = 0
  def madd(x: Int, y: Int): Int = x + y
}
def sum1(xa: List[Int]): Int =
  xa.foldLeft(intAdder.mzero)(intAdder.madd)      //> sum1: (xa: List[Int])Int
sum1(List(1,2,3))                                 //> res1: Int = 6

trait Addable[A] {
  def mzero: A
  def madd(x: A, y: A): A
}
case class Crew(names: List[String])
object Addable {
  implicit object intAddable extends Addable[Int] {
    val mzero = 0
    def madd(x: Int, y: Int) = x + y
  }
  implicit object strAddable extends Addable[String] {
    val mzero = ""
    def madd(x: String, y: String) = x + y
  }
  implicit object crewAddable extends Addable[Crew] {
    val mzero = Crew(List())
    def madd(x: Crew, y: Crew): Crew = Crew(x.names ++ y.names)
  }
  def apply[A](implicit M: Addable[A]): Addable[A] = M
}
def sum2[A](xa: List[A])(implicit M: Addable[A]): A =
  xa.foldLeft(M.mzero)(M.madd)                    //> sum2: [A](xa: List[A])(implicit M: demo.worksheet.typeclasses.Addable[A])A
  
sum2(List(1,2,3))                                 //> res2: Int = 6
sum2(List("ab","c","def"))                        //> res3: String = abcdef

sum2(List(Crew(List("john")), Crew(List("susan","peter"))))
                                                  //> res4: demo.worksheet.typeclasses.Crew = Crew(List(john, susan, peter))
Addable[Crew].mzero                               //> res5: demo.worksheet.typeclasses.Crew = Crew(List())
Addable[Crew].madd(Crew(List("john")), Crew(List("ada")))
                                                  //> res6: demo.worksheet.typeclasses.Crew = Crew(List(john, ada))
 
trait FoldLeft[F[_]] {
  def foldLeft[A,B](fa: F[A])(b: B)(f: (B,A) => B): B
}
object FoldLeft {
  implicit object listFold extends FoldLeft[List] {
    def foldLeft[A,B](fa: List[A])(b: B)(f: (B,A) => B) = fa.foldLeft(b)(f)
  }
  def apply[F[_]](implicit F: FoldLeft[F]): FoldLeft[F] = F
}
FoldLeft[List].foldLeft(List(1,2,3))(0)(_ + _)    //> res7: Int = 6

def sum3[A: Addable,F[_]: FoldLeft](fa: F[A]): A = {
  val adder = implicitly[Addable[A]]
  val folder = implicitly[FoldLeft[F]]
  folder.foldLeft(fa)(adder.mzero)(adder.madd)
}                                                 //> sum3: [A, F[_]](fa: F[A])(implicit evidence$1: demo.worksheet.typeclasses.A
                                                  //| ddable[A], implicit evidence$2: demo.worksheet.typeclasses.FoldLeft[F])A
sum3(List(Crew(List("john")), Crew(List("susan","peter"))))
                                                  //> res8: demo.worksheet.typeclasses.Crew = Crew(List(john, susan, peter))

/*
class AddableOp[A](a: A)(implicit M: Addable[A]) {
  def |+|(y: A) = M.madd(a,y)
}
implicit def toAddableOp[A: Addable](a: A): AddableOp[A] = new AddableOp[A](a)
*/
/*
implicit class addableOp[A: Addable](a: A) {
   def |+|(y: A) = implicitly[Addable[A]].madd(a,y)
}
*/
trait AddableOps[A] {
  val M: Addable[A]
  val x: A
  def |%| = M.mzero
  def |+|(y: A) = M.madd(x,y)
}
implicit def toAddableOps[A: Addable](a: A): AddableOps[A] = new AddableOps[A] {
  val M = implicitly[Addable[A]]
  val x = a
}                                                 //> toAddableOps: [A](a: A)(implicit evidence$3: demo.worksheet.typeclasses.Add
                                                  //| able[A])demo.worksheet.typeclasses.AddableOps[A]

3 |+| 2                                           //> res9: Int = 5
("hello" |+| " ") |+| "world!"                    //> res10: String = hello world!
3.|%|                                             //> res11: Int = 0
"hi".|%|                                          //> res12: String = ""

final class FoldLeftOps[F[_],A](self: F[A])(implicit F: FoldLeft[F]) {
   def foldl[B](z: B)(f: (B,A) => B) = F.foldLeft(self)(z)(f)
}
implicit def toFoldLeftOps[F[_]: FoldLeft, A](v: F[A]): FoldLeftOps[F,A] =
  new FoldLeftOps[F,A](v)                         //> toFoldLeftOps: [F[_], A](v: F[A])(implicit evidence$4: demo.worksheet.typec
                                                  //| lasses.FoldLeft[F])demo.worksheet.typeclasses.FoldLeftOps[F,A]
List(1,2,3).foldl(0)(_ + _)                       //> res13: Int = 6
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
}