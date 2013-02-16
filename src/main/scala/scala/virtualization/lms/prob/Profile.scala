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

  def __new[T](oargs: (String, Boolean, Exp[T] => Exp[_])*): Exp[T] = {
    val args = oargs.map(x => (x._1, x._3))
    val self = new Self[T](args.toMap)
    val argNames = args.toList.map(_._1)
    val evalArgs = argNames.map(x => x -> self(x))
    ProfileDef(evalArgs)
  }
  private class Self[T](members: Map[String, Exp[T] => Exp[_]]) extends Exp[T] {
    import scala.collection.mutable.{Map => MutMap}
    private val pending: MutMap[String, Exp[T] => Exp[_]] = MutMap(members.toSeq: _*)
    private val done: MutMap[String, Exp[_]] = MutMap.empty
    private def eval(member: String): Exp[_] = {
      val x = pending(member)(this)
      pending.remove(member)
      done.update(member, x)
      x
    }
    def apply(member: String): Exp[_] = done.getOrElseUpdate(member, eval(member))
  }
  abstract class ProfileOps {
    def selectDynamic[T](field: String): Exp[T]
  }
  class SelfOps[U](receiver: Self[U]) extends ProfileOps {
    def selectDynamic[T](field: String): Exp[T] = receiver(field).asInstanceOf[Exp[T]]
  }
  class ProfileOpsExp[U <: Profile](val receiver: Exp[U]) extends ProfileOps {
    def selectDynamic[T](field: String): Exp[T] = Select(receiver, field)
  }
  implicit def profileOps[U <: Profile](receiver: Exp[U]): ProfileOps = receiver match {
    case receiver: Self[_] => new SelfOps(receiver)
    case receiver =>          new ProfileOpsExp(receiver)
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
