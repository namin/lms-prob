package scala.virtualization.lms.prob

import scala.virtualization.lms.common._

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

trait ProbCore extends ProbIntf with ProbPrettyPrint {
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

trait DeepBase extends Base {
  type Rand[+A]
  type Prob = Double

  def flip(p: Prob = 0.5): Rep[Rand[Boolean]]
  def always[A:Manifest](e: Rep[A]): Rep[Rand[A]]
  def pp[A:Manifest](e: Rep[Rand[A]]): Rep[String]
}

trait DeepBaseExp extends DeepBase with EffectExp {
  class Rand[+A]

  case class Flip(p: Prob) extends Def[Rand[Boolean]]
  case class Always[A:Manifest](e: Exp[A]) extends Def[Rand[A]]
  case class PP[A:Manifest](e: Exp[Rand[A]]) extends Def[String]

  override def flip(p: Prob) = reflectEffect(Flip(p))
  override def always[A:Manifest](e: Exp[A]) = Always(e)
  override def pp[A:Manifest](e: Exp[Rand[A]]) = PP(e)
}

trait ScalaGenDeepBase extends ScalaGenEffect {
  val IR: DeepBaseExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Flip(p) => emitValDef(sym, "(flip(" + p + "))")
    case Always(e) => emitValDef(sym, "(always(" + quote(e) + "))")
    case PP(e) => emitValDef(sym, "(pp(" + quote(e) + "))")
    case _ => super.emitNode(sym, rhs)
  }
}

trait DeepLangBase extends DeepBase with IfThenElse with NumericOps with BooleanOps with Equal with ListOps with TupleOps with LiftNumeric with LiftBoolean
trait DeepLangExp extends DeepLangBase with DeepBaseExp with IfThenElseExp with NumericOpsExp with BooleanOpsExp with EqualExp with ListOpsExp with TupleOpsExp
trait ScalaGenDeepLang extends ScalaGenDeepBase with ScalaGenIfThenElse with ScalaGenNumericOps with ScalaGenBooleanOps with ScalaGenEqual with ScalaGenListOps with ScalaGenTupleOps {
  val IR: DeepLangExp
}
trait DeepLang extends DeepLangExp with CompileScala { q =>
  object codegen extends ScalaGenDeepLang {
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
  val f = (_: Rep[Unit]) => pp(flip())
  val fc = compile(f)
  println(fc())
}
