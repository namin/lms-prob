package scala.virtualization.lms.prob

import scala.virtualization.lms.common._
import scala.reflect.SourceContext

trait ProbIntf {
  type Prob = Double
  type Rand[+A] <: RandImpl[A]
  trait RandImpl[+A] {
    var name = super.toString
    override def toString = name
    def dbg(n:String): this.type = { name = n; this }
    def flatMap[B](f: A => Rand[B]): Rand[B]
    def map[B](f: A => B): Rand[B] = flatMap(x => always(f(x)))
    def orElse[B >: A](that: Rand[B]): Rand[B]
  }
  def always[A](x: A) = choice(x -> 1.0)
  def never = choice()
  def flip(p: Double): Rand[Boolean] = choice(true -> p, false -> (1-p))
  def uniform[A](xs: A*): Rand[A] = choice(xs.map((_,1.0)):_*)
  def choice[A](xs: (A,Prob)*): Rand[A]
  def collapse[A](r: Rand[A]): List[(A,Prob)]
}

trait ProbPrettyPrint extends ProbIntf {
  def pp[A](r: Rand[A]) = collapse(r).map{case (x,p) => x + " : " + p}.mkString("\n")
  def show[A](r: Rand[A], desc: String = "") = {
    println(desc)
    println(pp(r))
    println("")
  }
}

trait ProbLang extends EmbeddedControls with ProbIntf {

  def liftOp2[A,B,C](x: Rand[A], y: Rand[B])(f: (A,B) => C): Rand[C] = for (a <- x; b <- y) yield f(a,b)

  def infix_&&(x: Rand[Boolean], y: Rand[Boolean]): Rand[Boolean] = liftOp2(x,y)(_ && _) // short circuit ??
  def infix_===[A](x: Rand[A], y: Rand[A]): Rand[Boolean] =         liftOp2(x,y)(_ == _)
  def infix_+(x: Rand[Int], y: Rand[Int]): Rand[Int] =              liftOp2(x,y)(_ + _)

}

trait ProbCore extends ProbIntf with ProbPrettyPrint with ProbLang {
  type Rand[+A] = RandVar[A]
  case class Choice[+A](rv: Int, v: A, p: Prob)
  type Path = List[Choice[Any]]
  type Dist[+A] = List[Path]

  case class RandVar[+A](dist: Dist[A]) extends RandImpl[A] {
    def flatMap[B](f: A => Rand[B]): Rand[B] =
      RandVar(dist.flatMap(path => f(path.last.v.asInstanceOf[A]).dist.map(post => path ++ post)))
    def orElse[B >: A](that: Rand[B]): Rand[B] =
      RandVar(dist ++ that.dist)
  }

  def factor[A](w: Prob, xs: List[(A,Prob)]): List[(A,Prob)] = {
    xs.map{case (x,p) => (x,p*w)}
  }
  def consolidate[A](xs: List[(A,Prob)]): List[(A,Prob)] = {
    xs.filter(_._2 > 0).groupBy(_._1).map{case (x,ps) => (x, ps.map(_._2).sum)}.toList.sortBy{case (x,p) => (-p,x.toString)}
  }
  def normalize[A](xs: List[(A,Prob)]): List[(A,Prob)] = {
    val weight = xs.map(_._2).sum
    factor(1/weight,xs) // 1/0 ?
  }

  var numChoices = 0
  def freshChoiceId() = { numChoices += 1; numChoices - 1 }

  def choice[A](xs: (A,Prob)*): Rand[A] = {
    val id = freshChoiceId()
    RandVar[A](xs.toList.map{case(x,p) => List(Choice(id,x,p))})
  }

  def collapse[A](r: Rand[A]): List[(A,Prob)] = {
    def prob(path: Path, env: Map[Int,Any] = Map.empty): Prob = path match {
      case Choice(r,x,p)::rest =>
        env.get(r) match {
          case Some(`x`) => prob(rest,env)
          case None => p * prob(rest,env + (r -> x))
          case _ => 0
        }
      case _ => 1.0
    }
    normalize(consolidate(r.dist.map(path => (path.last.v, prob(path))))).asInstanceOf[List[(A,Prob)]]
  }
}

trait DeepBase extends Base with EmbeddedControls {
  type Rand[+A]
  type Prob = Double

  def flip(p: Prob = 0.5): Rep[Rand[Boolean]]
  def always[A:Manifest](e: Rep[A]): Rep[Rand[A]]
  def pp[A:Manifest](e: Rep[Rand[A]]): Rep[String]
  def __ifThenElse[T:Manifest](cond: Rep[Rand[Boolean]], thenp: => Rep[Rand[T]], elsep: => Rep[Rand[T]]): Rep[Rand[T]]
  def infix_&&(x: Rep[Rand[Boolean]], y: Rep[Rand[Boolean]]): Rep[Rand[Boolean]]
  def infix_===[A](x: Rep[Rand[A]], y: Rep[Rand[A]]): Rep[Rand[Boolean]]
  def infix_+(x: Rep[Rand[Int]], y: Rep[Rand[Int]]): Rep[Rand[Int]]
}

trait DeepBaseExp extends DeepBase with BaseExp {
  class Rand[+A]

  override protected implicit def toAtom[T:Manifest](d: Def[T])(implicit pos: SourceContext): Exp[T] = {
    d match {
      case Flip(_) =>
	val s = fresh[T](List(pos))
        createDefinition(s, d)
        s
      case _ => super.toAtom(d)
    }
  }

  case class Flip(p: Prob) extends Def[Rand[Boolean]]
  case class Always[A:Manifest](e: Exp[A]) extends Def[Rand[A]]
  case class PP[A:Manifest](e: Exp[Rand[A]]) extends Def[String]
  case class RandIfThenElse[T:Manifest](cond: Exp[Rand[Boolean]], thenp: Rep[Rand[T]], elsep: Rep[Rand[T]]) extends Def[Rand[T]]
  case class RandAnd(x: Rep[Rand[Boolean]], y: Rep[Rand[Boolean]]) extends Def[Rand[Boolean]]
  case class RandEq[A](x: Rep[Rand[A]], y: Rep[Rand[A]]) extends Def[Rand[Boolean]]
  case class RandPlus(x: Rep[Rand[Int]], y: Rep[Rand[Int]]) extends Def[Rand[Int]]

  override def flip(p: Prob) = Flip(p)
  override def always[A:Manifest](e: Exp[A]) = Always(e)
  override def pp[A:Manifest](e: Exp[Rand[A]]) = PP(e)
  override def __ifThenElse[T:Manifest](cond: Rep[Rand[Boolean]], thenp: => Rep[Rand[T]], elsep: => Rep[Rand[T]]): Rep[Rand[T]] = RandIfThenElse(cond, thenp, elsep)
  override def infix_&&(x: Rep[Rand[Boolean]], y: Rep[Rand[Boolean]]): Rep[Rand[Boolean]] = RandAnd(x, y)
  override def infix_===[A](x: Rep[Rand[A]], y: Rep[Rand[A]]): Rep[Rand[Boolean]] = RandEq(x, y)
  override def infix_+(x: Rep[Rand[Int]], y: Rep[Rand[Int]]): Rep[Rand[Int]] = RandPlus(x, y)
}

trait DeepBaseExpOpt extends DeepBaseExp {
  override def infix_===[A](x: Rep[Rand[A]], y: Rep[Rand[A]]): Rep[Rand[Boolean]] = {
    x match {
      case Def(RandIfThenElse(Def(Flip(p)), Def(a), Def(b))) =>
      println("x is an " + List(p, a, b))
    }
    super.infix_===(x, y)
  }
}

trait ScalaGenDeepBase extends ScalaGenBase {
  val IR: DeepBaseExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Flip(p) => emitValDef(sym, "(flip(" + p + "))")
    case Always(e) => emitValDef(sym, "(always(" + quote(e) + "))")
    case PP(e) => emitValDef(sym, "(pp(" + quote(e) + "))")
    case RandIfThenElse(c,a,b) =>
      stream.println("val " + quote(sym) + " = " + quote(c) + ".flatMap{ c => c match {")
      stream.println("case true =>" + quote(a))
      stream.println("case false =>" + quote(b))
      stream.println("}}")
    case RandAnd(x, y) => emitValDef(sym, quote(x) + " && " + quote(y))
    case RandEq(x, y) => emitValDef(sym, quote(x) + " === " + quote(y))
    case RandPlus(x, y) => emitValDef(sym, quote(x) + " + " + quote(y))
    case _ => super.emitNode(sym, rhs)
  }
}

trait DeepLangBase extends DeepBase/* with IfThenElse with NumericOps with BooleanOps with Equal with ListOps with TupleOps with LiftNumeric with LiftBoolean*/
trait DeepLangExp extends DeepLangBase with DeepBaseExp with DeepBaseExpOpt /* with IfThenElseExp with NumericOpsExp with BooleanOpsExp with EqualExp with ListOpsExp with TupleOpsExp*/
trait ScalaGenDeepLang extends ScalaGenDeepBase/* with ScalaGenIfThenElse with ScalaGenNumericOps with ScalaGenBooleanOps with ScalaGenEqual with ScalaGenListOps with ScalaGenTupleOps*/ {
  val IR: DeepLangExp
}
trait DeepLang extends DeepLangExp with EffectExp with CompileScala { q =>
  object codegen extends ScalaGenDeepLang with ScalaGenEffect {
    val IR: q.type = q

    override def remap[A](m: Manifest[A]): String = {
      val r = super.remap(m)
      r.replace("scala.virtualization.lms.prob.DeepBaseExp$Rand", "Rand")
    }
  }
  override def compile[A,B](f: Exp[A] => Exp[B])(implicit mA: Manifest[A], mB: Manifest[B]): A=>B = {
    compile(f, Some("scala.virtualization.lms.prob.ProbCore"))(mA, mB)
  }
}

object TestDeep extends App with DeepLang {
  val f1 = (_: Rep[Unit]) => pp(flip())
  val fc1 = compile(f1)
  println(fc1())

  val f2 = (_: Rep[Unit]) => pp(if (flip()) always(unit(1)) else always(unit(0)))
  val fc2 = compile(f2)
  println(fc2())

  val f3 = (_: Rep[Unit]) => {
    val a = if (flip()) always(unit(1)) else always(unit(0))
    val b = if (flip()) always(unit(1)) else always(unit(0))
    pp(a === b)
  }
  val fc3 = compile(f3)
  println(fc3())

  val f4 = (_: Rep[Unit]) => {
    val a = if (flip()) always(unit(1)) else always(unit(0))
    val b = a
    pp(a === b)
  }
  val fc4 = compile(f4)
  println(fc4())
}
