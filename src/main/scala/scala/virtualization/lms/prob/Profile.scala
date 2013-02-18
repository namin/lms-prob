package scala.virtualization.lms.prob

object TestProfile extends App with EmbeddedControls {
  type Prob = Double

  abstract class Exp[+T]
  case class Flip(id: Int, p: Prob) extends Exp[Boolean]
  case class Binomial(id: Int, p: Prob, n: Int) extends Exp[Int]
  case class Always[A](e: Exp[A]) extends Exp[A]
  case class Plus(x: Exp[Int], y: Exp[Int]) extends Exp[Int]
  case class Equals[A](x: Exp[A], y: Exp[A]) extends Exp[Boolean]
  case class IfThenElse[T](x: Exp[Boolean], y: Exp[T], z: Exp[T]) extends Exp[T]

  trait Profile extends Struct[Exp]
  case class ProfileDef[R <: Profile](fields: List[(String, Exp[_])]) extends Exp[R]
  case class Select[T, U](tgt: Exp[U], field: String) extends Exp[T]

  def __new[T](args: (String, Boolean, Exp[T] => Exp[_])*): Exp[T] = {
    val self = new Self[T](args map{case (n, _, e) => (n -> e)} toMap)
    ProfileDef(args map {case (n, _, _) => (n -> self(n))} toList)
  }
  private class Self[T](members: Map[String, Exp[T] => Exp[_]]) extends Exp[T] {
    private val cache = scala.collection.mutable.Map.empty[String, Exp[_]]
    def apply(name: String): Exp[_] = cache.getOrElseUpdate(name, members(name)(this))
  }
  implicit class ProfileOps[U <: Profile](receiver: Exp[U]) {
    def selectDynamic[T](field: String): Exp[T] = receiver match {
      case self: Self[_] => self(field).asInstanceOf[Exp[T]]
      case _             => Select(receiver, field)
    }
  }

  var id = 0
  def freshId() = { id += 1; id }

  def flip(p: Prob)              = Flip(freshId(), p)
  def binomial(p: Prob, n: Int)  = Binomial(freshId(), p, n)
  def always[A](e: Exp[A])       = Always(e)
  def infix_+(x: Exp[Int], y: Exp[Int]): Exp[Int]                  = Plus(x, y)
  def infix_===[A](x: Exp[A], y: Exp[A]): Exp[Boolean]             = Equals(x, y)
  def __ifThenElse[T](x: Exp[Boolean], y: => Exp[T], z: => Exp[T]) = IfThenElse(x,y,z)

  val p = new Profile {
    val good: Exp[Int]    = binomial(0.5, 3)
    val healthy: Exp[Int] = binomial(0.5, 3)
    val happy: Exp[Int]   = good+healthy
  }
  println(p)
  println(p.happy)
}
