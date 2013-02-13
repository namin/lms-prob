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
  def infix_**(base: Prob, exponent: Double): Prob = scala.math.pow(base, exponent)
  def combinations(n: Int, k: Int): Int =
    ((1 to n).product / ((1 to k).product * (1 to (n - k)).product))
  def bernouilli(p: Prob, n: Int): Rand[Int] =
    choice((for (k <- 0 to n) yield ((k -> combinations(n, k) * (p**k) * ((1-p)**(n-k))))):_*)
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
  def bernouilli(p: Prob, n: Int): Rep[Rand[Boolean]]
  def always[A:Manifest](e: Rep[A]): Rep[Rand[A]]
  def pp[A:Manifest](e: Rep[Rand[A]]): Rep[String]
  def __ifThenElse[T:Manifest](cond: Rep[Rand[Boolean]], thenp: => Rep[Rand[T]], elsep: => Rep[Rand[T]]): Rep[Rand[T]]
  def infix_&&(x: Rep[Rand[Boolean]], y: Rep[Rand[Boolean]]): Rep[Rand[Boolean]]
  def infix_===[A](x: Rep[Rand[A]], y: Rep[Rand[A]]): Rep[Rand[Boolean]]
  def infix_+[T:Numeric:Manifest](x: Rep[Rand[T]], y: Rep[Rand[T]]): Rep[Rand[T]]

  class RepRandNum[T:Numeric:Manifest] extends Numeric[Rep[Rand[T]]] {
    val t = implicitly[Numeric[T]]
    type R = Rep[Rand[T]]
    def compare(x: R, y: R): Int = ???
    def fromInt(x: Int) = always(unit(t.fromInt(x)))
    def minus(x: R, y: R): R = ???
    def negate(x: R): R = ???
    def plus(x: R, y: R) = infix_+(x, y)
    def times(x: R, y: R): R = ???
    def toDouble(x: R): Double = ???
    def toFloat(x: R): Float = ???
    def toInt(x: R): Int = ???
    def toLong(x: R): Long = ???
  }
  implicit def repRandNum[T:Numeric:Manifest]: Numeric[Rep[Rand[T]]] = new RepRandNum[T]()
}

trait DeepBaseExp extends DeepBase with EffectExp {
  class Rand[+A]

  case class Flip(p: Prob) extends Def[Rand[Boolean]]
  case class Bernouilli(p: Prob, n: Int) extends Def[Rand[Boolean]]
  case class Always[A:Manifest](e: Exp[A]) extends Def[Rand[A]]
  case class PP[A:Manifest](e: Exp[Rand[A]]) extends Def[String]
  case class RandIfThenElse[T:Manifest](cond: Exp[Rand[Boolean]], thenp: Block[Rand[T]], elsep: Block[Rand[T]]) extends Def[Rand[T]]
  case class RandAnd(x: Rep[Rand[Boolean]], y: Rep[Rand[Boolean]]) extends Def[Rand[Boolean]]
  case class RandEq[A](x: Rep[Rand[A]], y: Rep[Rand[A]]) extends Def[Rand[Boolean]]
  case class RandPlus[T:Numeric:Manifest](x: Rep[Rand[T]], y: Rep[Rand[T]]) extends Def[Rand[T]]

  override def flip(p: Prob) = reflectEffect(Flip(p), Alloc())
  override def bernouilli(p: Prob, n: Int) = reflectEffect(Bernouilli(p, n), Alloc())
  override def always[A:Manifest](e: Exp[A]) = Always(e)
  override def pp[A:Manifest](e: Exp[Rand[A]]) = PP(e)
  override def __ifThenElse[T:Manifest](cond: Rep[Rand[Boolean]], thenp: => Rep[Rand[T]], elsep: => Rep[Rand[T]]): Rep[Rand[T]] = {
    val a = reifyEffectsHere(thenp)
    val b = reifyEffectsHere(elsep)

    RandIfThenElse(cond,a,b)
  }
  override def infix_&&(x: Rep[Rand[Boolean]], y: Rep[Rand[Boolean]]): Rep[Rand[Boolean]] = RandAnd(x, y)
  override def infix_===[A](x: Rep[Rand[A]], y: Rep[Rand[A]]): Rep[Rand[Boolean]] = RandEq(x, y)
  override def infix_+[T:Numeric:Manifest](x: Rep[Rand[T]], y: Rep[Rand[T]]): Rep[Rand[T]] = RandPlus(x, y)
}

trait ScalaGenDeepBase extends ScalaGenEffect {
  val IR: DeepBaseExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Flip(p) => emitValDef(sym, "(flip(" + p + "))")
    case Bernouilli(p, n) => emitValDef(sym, "(bernouilli(" + p + ", " + n + "))")
    case Always(e) => emitValDef(sym, "(always(" + quote(e) + "))")
    case PP(e) => emitValDef(sym, "(pp(" + quote(e) + "))")
    case RandIfThenElse(c,a,b) =>
      stream.println("val " + quote(sym) + " = " + quote(c) + ".flatMap{ c => c match {")
      stream.println("case true =>")
      emitBlock(a)
      stream.println(quote(getBlockResult(a)))
      stream.println("case false =>")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}}")
    case RandAnd(x, y) => emitValDef(sym, quote(x) + " && " + quote(y))
    case RandEq(x, y) => emitValDef(sym, quote(x) + " === " + quote(y))
    case RandPlus(x, y) => emitValDef(sym, quote(x) + " + " + quote(y))
    case _ => super.emitNode(sym, rhs)
  }
}

trait DeepLangBase extends DeepBase/* with IfThenElse with NumericOps with BooleanOps with Equal with ListOps with TupleOps with LiftNumeric with LiftBoolean*/
trait DeepLangExp extends DeepLangBase with DeepBaseExp/* with IfThenElseExp with NumericOpsExp with BooleanOpsExp with EqualExp with ListOpsExp with TupleOpsExp*/
trait ScalaGenDeepLang extends ScalaGenDeepBase/* with ScalaGenIfThenElse with ScalaGenNumericOps with ScalaGenBooleanOps with ScalaGenEqual with ScalaGenListOps with ScalaGenTupleOps*/ {
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

  val f5 = (_: Rep[Unit]) => {
    val coins = for (i <- 0 until 10) yield flip()
    val sum = coins.map(c => if (c) always(unit(1)) else always(unit(0))).sum
    val allheads = sum === always(unit(10))
    pp(allheads)
  }
  val fc5 = compile(f5)
  println(fc5())

  val f6 = (_: Rep[Unit]) => {
    val sum = bernouilli(0.5, 10)
    val allheads = sum === always(unit(10))
    pp(allheads)
  }
  val fc6 = compile(f6)
  println(fc6())

  val f7 = (_: Rep[Unit]) => {
    val coins = for (i <- 0 until 10) yield flip()
    val sum = coins.map(c => if (c) always(unit(1)) else always(unit(0))).sum
    val allheads = sum === always(unit(5))
    pp(allheads)
  }
  val fc7 = compile(f7)
  println(fc7())

  val f8 = (_: Rep[Unit]) => {
    val sum = bernouilli(0.5, 10)
    val allheads = sum === always(unit(5))
    pp(allheads)
  }
  val fc8 = compile(f8)
  println(fc8())
}
