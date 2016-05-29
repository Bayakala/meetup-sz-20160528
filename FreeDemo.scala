package demo.app
import scalaz._
import Scalaz._
import scala.language.implicitConversions
import scala.language.higherKinds
object FreeADTs {
  sealed trait Interact[+A]
  case class Ask[A](prompt: String, onInput: String => A) extends Interact[A]
  case class Tell[A](msg: String, next: A) extends Interact[A]
  sealed trait InteractInstances {
    object InteractFunctor extends Functor[Interact] {
      def map[A,B](ia: Interact[A])(f: A => B): Interact[B] = ia match {
        case Ask(prompt,input) => Ask(prompt, input andThen f)
        case Tell(msg,next) => Tell(msg, f(next))
      }
    }
  }
  sealed trait InteractFunctions {
    def ask[G[_],A](p: String, f: String => A)(implicit I: Inject[Interact,G]): Free[G,A] =
      Free.liftF(I.inj(Ask(p,f)))
    def tell[G[_]](m: String)(implicit I: Inject[Interact,G]): Free[G,Unit] =
      Free.liftF(I.inj(Tell(m,Free.pure(()))))
  }
  object Interacts extends InteractInstances with InteractFunctions

  sealed trait UserLogin[+A]  // None Functor 高阶类
  case class CheckId(uid: String) extends UserLogin[Boolean]
  case class Login(uid: String, pswd: String) extends UserLogin[Boolean]
  sealed trait LoginFunctions {
    def checkId[G[_]](uid: String)(implicit I: Inject[UserLogin,G]): Free[G,Boolean] =
      Free.liftF(I.inj(CheckId(uid)))
    def login[G[_]](uid: String, pswd: String)(implicit I: Inject[UserLogin, G]): Free[G,Boolean] =
      Free.liftF(I.inj(Login(uid,pswd)))
  }
  object Logins extends LoginFunctions
  sealed trait Permission[+A]
  case class HasPermission(uid: String, acc: Int) extends Permission[Boolean]
  sealed trait PermissionFunctions {
    def hasPermission[G[_]](uid: String, acc: Int)(implicit I: Inject[Permission,G]): Free[G,Boolean] =
      Free.liftF(I.inj(HasPermission(uid,acc)))
  }
  object Permissions extends PermissionFunctions
}
object FreeASTs {
  import FreeADTs._
  import Interacts._
  val interactScript = for {
    first <- ask("what's your first name?",identity)
    last <- ask("your last name?",_.toUpperCase())
    _ <- tell(s"hello, $first $last")
  } yield ()
  import Logins._
  type InteractLogin[A] = Coproduct[Interact,UserLogin,A]
  val loginScript = for {
    uid <- ask[InteractLogin,String]("what's you id?",identity)
    idok <- checkId[InteractLogin](uid)
    _ <- if (idok) tell[InteractLogin](s"hi, $uid") else tell[InteractLogin]("sorry, don't know you!")
    pwd <- if (idok) ask[InteractLogin,String](s"what's your password?",identity)
    else Free.point[InteractLogin,String]("")
    login <- if (idok) login[InteractLogin](uid,pwd)
    else Free.point[InteractLogin,Boolean](false)
    _ <- if (login) tell[InteractLogin](s"congratulations，$uid")
    else tell[InteractLogin](idok ? "sorry, no pass!" | "")
  } yield login
  import Permissions._
  type InteractLoginPermission[A] = Coproduct[Permission,InteractLogin,A]
  type T[A] = InteractLoginPermission[A]
  val authScript = for {
    uid <- ask[T,String]("what's you id?",identity)
    idok <- checkId[T](uid)
    _ <- if (idok) tell[T](s"hi, $uid")
    else tell[T]("sorry, don't know you!")
    pwd <- if (idok) ask[T,String](s"what's your password?",identity)
    else Free.point[T,String]("")
    login <- if (idok) login[T](uid,pwd)
    else Free.point[T,Boolean](false)
    _ <- if (login) tell[T](s"congratulations，$uid")
    else tell[T](idok ? "sorry, no pass!" | "")
    acc <- if (login) ask[T,Int](s"what level of access, $uid?",_.toInt)
    else Free.point[T,Int](0)
    perm <- if (login) hasPermission[T](uid,acc)
    else Free.point[T,Boolean](false)
    _ <- if (perm) tell[T](s"you may use the system，$uid")
    else tell[T]((idok && login)  ? "sorry, it's above your pay grade!" | "")

  } yield ()
}
object FreeInterps {
  import FreeADTs._
  object InteractConsole extends (Interact ~> Id) {
    def apply[A](ia: Interact[A]): Id[A] = ia match {
      case Ask(p,onInput) => println(p); onInput(readLine)
      case Tell(m,n) => println(m); n
    }
  }
  import Dependencies._
  type AuthReader[A] = Reader[Authenticator,A]
  object InteractLogin extends (Interact ~> AuthReader) {
    def apply[A](ia: Interact[A]): AuthReader[A] = ia match {
      case Ask(p,onInput) => println(p); Reader {m => onInput(readLine)}
      case Tell(msg,n) => println(msg); Reader {m => n}
    }
  }
  object LoginConsole extends (UserLogin ~> AuthReader) {
    def apply[A](ua: UserLogin[A]): AuthReader[A] = ua match {
      case CheckId(uid) => Reader {m => m.validateId(uid)}
      case Login(uid,pwd) => Reader {m => m.validatePassword(uid, pwd)}
    }
  }
  object PermConsole extends (Permission ~> AuthReader) {
    def apply[A](pa: Permission[A]): AuthReader[A] = pa match {
      case HasPermission(uid,acc) => Reader {m => m.grandAccess(uid, acc)}
    }
  }
  def or[F[_],H[_],G[_]](f: F~>G, h: H~>G) =
    new (({type l[x] = Coproduct[F,H,x]})#l ~> G) {
      def apply[A](ca: Coproduct[F,H,A]):G[A] = ca.run match {
        case -\/(fg) => f(fg)
        case \/-(hg) => h(hg)
      }
    }
  def among3[F[_],H[_],K[_],G[_]](f: F~>G, h: H~>G, k: K~>G) = {
    type FH[A] = Coproduct[F,H,A]
    type KFH[A] = Coproduct[K,FH,A]
    new (({type l[x] = Coproduct[K,FH,x]})#l ~> G) {
      def apply[A](kfh: KFH[A]): G[A] = kfh.run match {
        case -\/(kg) => k(kg)
        case \/-(cfh) => cfh.run match {
          case -\/(fg) => f(fg)
          case \/-(hg) => h(hg)
        }
      }
    }
  }
}
object Dependencies {
  trait UserControl {
    val pswdMap: Map[String,String]
    def validateId(uid: String): Boolean
    def validatePassword(uid: String, pswd: String): Boolean
  }
  trait AccessControl {
    val accMap: Map[String, Int]
    def grandAccess(uid: String, acc: Int): Boolean
  }
  trait Authenticator extends UserControl with AccessControl
}
object FreeDemo extends App {
  import FreeASTs._
  import FreeInterps._
  import Dependencies._
  object AuthControl extends Authenticator {
    val pswdMap = Map (
      "Tiger" -> "1234",
      "John" -> "0000"
    )
    override def validateId(uid: String) =
      pswdMap.getOrElse(uid,"???") /== "???"
    override def validatePassword(uid: String, pswd: String) =
      pswdMap.getOrElse(uid, pswd+"!") === pswd

    val accMap = Map (
      "Tiger" -> 8,
      "John" -> 0
    )
    override def grandAccess(uid: String, acc: Int) =
      accMap.getOrElse(uid, -1) > acc
  }
    authScript.foldMapRec(among3(InteractLogin,LoginConsole,PermConsole)).run(AuthControl)
  //  loginScript.foldMapRec(or(InteractLogin,LoginConsole)).run(AuthControl)
  // interactScript.foldMapRec(InteractConsole)

}