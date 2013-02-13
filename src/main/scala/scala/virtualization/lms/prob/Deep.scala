package scala.virtualization.lms.prob

import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import java.io.PrintWriter

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
  def bernoulli(p: Prob, n: Int): Rand[Int] =
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
  def bernoulli(p: Prob, n: Int): Rep[Rand[Int]]
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
  case class Bernoulli(p: Prob, n: Int) extends Def[Rand[Int]]
  case class Always[A:Manifest](e: Exp[A]) extends Def[Rand[A]]
  case class PP[A:Manifest](e: Exp[Rand[A]]) extends Def[String]
  case class RandIfThenElse[T:Manifest](cond: Exp[Rand[Boolean]], thenp: Block[Rand[T]], elsep: Block[Rand[T]]) extends Def[Rand[T]]
  case class RandAnd(x: Rep[Rand[Boolean]], y: Rep[Rand[Boolean]]) extends Def[Rand[Boolean]]
  case class RandEq[A](x: Rep[Rand[A]], y: Rep[Rand[A]]) extends Def[Rand[Boolean]]
  case class RandPlus[T:Numeric:Manifest](x: Rep[Rand[T]], y: Rep[Rand[T]]) extends Def[Rand[T]] {
    def mev = manifest[T]
    def aev = implicitly[Numeric[T]]
  }

  override def flip(p: Prob) = reflectEffect(Flip(p), Alloc())
  override def bernoulli(p: Prob, n: Int) = reflectEffect(Bernoulli(p, n), Alloc())
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

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = e match {
    case Flip(p) => Flip(p).asInstanceOf[Def[A]]
    case Bernoulli(p, n) => Bernoulli(p, n).asInstanceOf[Def[A]]
    case RandIfThenElse(c,a,b) => RandIfThenElse(f(c),f(a),f(b)).asInstanceOf[Def[A]]
    case Always(e) => Always(f(e)).asInstanceOf[Def[A]]
    case PP(e) => PP(f(e)).asInstanceOf[Def[A]]
    case RandAnd(x, y) => RandAnd(f(x), f(y)).asInstanceOf[Def[A]]
    case RandEq(x, y) => RandEq(f(x), f(y)).asInstanceOf[Def[A]]
    case e@RandPlus(x, y) => RandPlus(f(x), f(y))(e.aev, mtype(e.mev)).asInstanceOf[Def[A]]
    case _ => super.mirrorDef(e,f)
  }
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
    case Always(e) => always(f(e)).asInstanceOf[Exp[A]]
    case PP(e) => pp(f(e)).asInstanceOf[Exp[A]]
    case RandAnd(x, y) => infix_&&(f(x), f(y)).asInstanceOf[Exp[A]]
    case RandEq(x, y) => infix_===(f(x), f(y)).asInstanceOf[Exp[A]]
    case e@RandPlus(x, y) => infix_+(f(x), f(y))(e.aev, mtype(e.mev)).asInstanceOf[Exp[A]]
    case Reflect(Flip(p), u, es) =>
        reflectMirrored(Reflect(Flip(p), mapOver(f,u), f(es)))(mtype(manifest[A])).asInstanceOf[Exp[A]]
    case Flip(p) =>
        Flip(p).asInstanceOf[Def[A]]
    case Reflect(Bernoulli(p, n), u, es) =>
        reflectMirrored(Reflect(Bernoulli(p, n), mapOver(f,u), f(es)))(mtype(manifest[A])).asInstanceOf[Exp[A]]
    case Bernoulli(p, n) =>
        Bernoulli(p, n).asInstanceOf[Def[A]]
    case Reflect(RandIfThenElse(c,a,b), u, es) =>
        reflectMirrored(Reflect(RandIfThenElse(f(c),f(a),f(b)), mapOver(f,u), f(es)))(mtype(manifest[A])).asInstanceOf[Exp[A]]
    case RandIfThenElse(c,a,b) =>
        RandIfThenElse(f(c),f(a),f(b)).asInstanceOf[Def[A]]
    case _ => super.mirror(e,f)
  }
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case RandIfThenElse(c,a,b) => syms(a):::syms(b)
    case _ => super.aliasSyms(e)
  }
  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case RandIfThenElse(c,a,b) => Nil
    case _ => super.containSyms(e)
  }
  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case RandIfThenElse(c,a,b) => Nil
    case _ => super.extractSyms(e)
  }
  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case RandIfThenElse(c,a,b) => Nil // could return a,b but implied by aliasSyms
    case _ => super.copySyms(e)
  }
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case RandIfThenElse(c, t, e) => freqNormal(c) ++ freqCold(t) ++ freqCold(e)
    case _ => super.symsFreq(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case RandIfThenElse(c, t, e) => effectSyms(t):::effectSyms(e)
    case _ => super.boundSyms(e)
  }
}

trait ScalaGenDeepBase extends ScalaGenEffect {
  val IR: DeepBaseExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Flip(p) => emitValDef(sym, "(flip(" + p + "))")
    case Bernoulli(p, n) => emitValDef(sym, "(bernoulli(" + p + ", " + n + "))")
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
trait DeepLangExp extends DeepLangBase with DeepBaseExp with BaseFatExp /* with IfThenElseExp with NumericOpsExp with BooleanOpsExp with EqualExp with ListOpsExp with TupleOpsExp*/
trait ScalaGenDeepLang extends ScalaGenDeepBase/* with ScalaGenIfThenElse with ScalaGenNumericOps with ScalaGenBooleanOps with ScalaGenEqual with ScalaGenListOps with ScalaGenTupleOps*/ {
  val IR: DeepLangExp
}

trait ProbTransformer extends RecursiveTransformer {
  val IR: DeepLangExp
  import IR._

  val defUseMap : scala.collection.mutable.Map[Sym[_], Int] = new scala.collection.mutable.HashMap
  def addDefUse(sym: Sym[_]) = defUseMap.update(sym, defUseMap.getOrElse(sym, 0) + 1)
  def defUse(sym: Sym[_]): Int = defUseMap.getOrElse(sym, 0)
  def defUse(e: Exp[_]): Int = defUse(e.asInstanceOf[Sym[_]])

  def buildDefUse(body: Block[Any]): Unit = {
    defUseMap.clear()

    refSyms(getBlockResult(body)).foreach(addDefUse)
    for (TP(_, rhs) <- buildScheduleForResult(body, false /*unsorted*/)) {
      for (sym <- refSyms(rhs)) {
	addDefUse(sym)
      }
    }
  }

  def refSyms(e: Any): List[Sym[Any]] = e match {
    case Reify(x, u, es) => readSyms(x)
    case _ => readSyms(e)
  }

  var bernoulliRewrites = 0
  def printSummary() = {
    println("Bernoulli Rewrites: " + bernoulliRewrites)
  }

  override def transformDef[A](lhs: Sym[A], rhs: Def[A]) = {
    rhs match {
      case Reflect(RandIfThenElse(f@Def(Reflect(Flip(p), _, _)), a@Block(Def(Always(Const(1)))), b@Block(Def(Always(Const(0))))), u, es) if defUse(lhs) < 2 && defUse(f) < 2 =>
        bernoulliRewrites += 1
        Some(() => Reflect(Bernoulli(p, 1).asInstanceOf[Def[A]], u, es))
      case _ => None
    }
  }
}

trait DeepLang extends DeepLangExp with CompileScala { q =>
  object codegen extends ScalaGenDeepLang {
    val IR: q.type = q

    override def remap[A](m: Manifest[A]): String = {
      val r = super.remap(m)
      r.replace("scala.virtualization.lms.prob.DeepBaseExp$Rand", "Rand")
    }

    override def emitSource[T : Manifest, R : Manifest](f: Exp[T] => Exp[R], className: String, stream: PrintWriter, obj: Option[String]): List[(Sym[Any], Any)] = {
      val s = fresh[T]
      val body = reifyBlock(f(s))
      val trans = new ProbTransformer {
	val IR: q.type = q
      }
      trans.buildDefUse(body)
      val transBody = trans.transformBlock(body)
      trans.printSummary()
      emitSource(List(s), transBody, className, stream, obj)
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
    val sum = bernoulli(0.5, 10)
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
    val sum = bernoulli(0.5, 10)
    val allheads = sum === always(unit(5))
    pp(allheads)
  }
  val fc8 = compile(f8)
  println(fc8())
}
