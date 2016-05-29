package demo.worksheet

object polym {
object overloading {
  case class Color(scheme: String)
  case class Person(name: String)
  def tell(color: Color) = s"I'm Color ${color.scheme}"
  def tell(person: Person) = s"I'm ${person.name}"
}
/*import overloading._
tell(Color("RED"))
tell(Person("John"))
*/
object inheritance {
  trait AnyThing
  case class Color(scheme: String) extends AnyThing {
    def tell: String = s"I'm Color ${scheme}"
  }
  case class Person(name: String) extends AnyThing {
    def tell: String = s"I'm ${name}"
  }
}
/*import inheritance._
Color("RED").tell
Person("John").tell
*/
object patternmatch {
  case class Color(scheme: String)
  case class Person(name: String)
  def tell(a: Any): String = a match {
     case Color(sch) =>  s"I'm Color ${sch}"
     case Person(nm) => s"I'm ${nm}"
     case i: Int => s"I'm a Integer with value $i"
  }
}
/*
import patternmatch._
tell(3)
tell(Color("RED"))
tell(Person("Jonh"))
*/

object typeclasses {
  case class Color(scheme: String)
  case class Person(name: String)
  trait Tellable[A] {
    def tell(a: A): String
  }
  object Tellable {
    implicit object StringTellable extends Tellable[String] {
       def tell(s: String): String =  s"I'm a string of chars $s"
    }
    implicit val intTellable = new Tellable[Int] {
       def tell(i: Int): String = s"I'm integer $i"
    }
    implicit def listTellable = new Tellable[List[Person]] {
       def tell(xp: List[Person]) =
        (xp.map(p => p.name)).mkString("I am list of persons [",",","]")
    }
  }
  def tellAll[A](a: A)(implicit Teller: Tellable[A]): String = Teller.tell(a)
}
import typeclasses._
tellAll("hello world!")                           //> res0: String = I'm a string of chars hello world!
tellAll(64)                                       //> res1: String = I'm integer 64
implicit val colorTellable = new Tellable[Color] {
   def tell(c: Color): String = s"I'm Color ${c.scheme}"
}                                                 //> colorTellable  : demo.worksheet.polym.typeclasses.Tellable[demo.worksheet.p
                                                  //| olym.typeclasses.Color] = demo.worksheet.polym$$anonfun$main$1$$anon$3@735f
                                                  //| 7ae5
implicit val personTellable = new Tellable[Person] {
   def tell(p: Person): String = s"I'm $p.name"
}                                                 //> personTellable  : demo.worksheet.polym.typeclasses.Tellable[demo.worksheet.
                                                  //| polym.typeclasses.Person] = demo.worksheet.polym$$anonfun$main$1$$anon$4@18
                                                  //| 0bc464
tellAll(Color("Red"))                             //> res2: String = I'm Color Red
tellAll(Person("John"))                           //> res3: String = I'm Person(John).name

tellAll(List(Person("Peter"),Person("John"),Person("Cat"),Person("Chris")))
                                                  //> res4: String = I am list of persons [Peter,John,Cat,Chris]

implicit val myIntTellable = new  Tellable[Int] {
  def tell(i: Int): String = s"my integer value is $i"
}                                                 //> myIntTellable  : demo.worksheet.polym.typeclasses.Tellable[Int] = demo.work
                                                  //| sheet.polym$$anonfun$main$1$$anon$5@34b7bfc0

tellAll(24)                                       //> res5: String = my integer value is 24
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
}