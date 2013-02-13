package scala.virtualization.lms.prob

import scala.virtualization.lms.common._

trait ChurchBase extends Base {
  type Prob = Double
  def flip(p: Rep[Prob] = unit(0.5)): Rep[Boolean]
}

trait ChurchBaseExp extends ChurchBase with EffectExp {
  case class Flip(p: Exp[Prob]) extends Def[Boolean]
  override def flip(p: Rep[Prob]) = reflectEffect(Flip(p))
}

trait ScalaGenChurchBase extends ScalaGenEffect {
  val IR: ChurchBaseExp
  import IR._

  def utilRandom: String

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Flip(p) => emitValDef(sym, "(" + utilRandom + ".nextDouble() < " + quote(p) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ChurchLangBase extends ChurchBase with IfThenElse with NumericOps with BooleanOps with Equal with ListOps with TupleOps with LiftNumeric with LiftBoolean
trait ChurchLangExp extends ChurchLangBase with ChurchBaseExp with IfThenElseExp with NumericOpsExp with BooleanOpsExp with EqualExp with ListOpsExp with TupleOpsExp
trait ScalaGenChurchLang extends ScalaGenChurchBase with ScalaGenIfThenElse with ScalaGenNumericOps with ScalaGenBooleanOps with ScalaGenEqual with ScalaGenListOps with ScalaGenTupleOps {
  val IR: ChurchLangExp

  override def utilRandom: String = "(new scala.util.Random())"
}
trait ChurchLang extends ChurchLangExp with CompileScala { q =>
  object codegen extends ScalaGenChurchLang {
    val IR: q.type = q
  }
  def query[P:Manifest,R:Manifest](program: Rep[Unit] => Rep[P], result: Rep[P] => Rep[R], predicate: Rep[P] => Rep[Boolean]): (Unit => R) = {
    val f_program = compile(program)
    val f_result = compile(result)
    val f_predicate = compile(predicate)

    val ret = (_: Unit) => {
      var p = f_program()
      while (!f_predicate(p)) {
	println("discarding " + p)
	p = f_program()
      }
      f_result(p)
    }
    ret
  }
  def repeat[R](n: Int, f: Unit => R): List[R] = {
    (for (i <- 0 until n) yield f()).toList
  }
  def hist[R](xs: List[R]): Map[R, Int] = {
    xs.groupBy(x => x).mapValues(_.size)
  }
}

object TestChurch extends App with ChurchLang {
  println("q1")
  val q1 = query(
    (_ : Rep[Unit]) => flip(),
    (x: Rep[Boolean]) => x,
    (x: Rep[Boolean]) => unit(true))
  println("1:" + q1())
  println("2:" + q1())
  println("1 repeated 10:" + hist(repeat(10, q1)))
  println("2 repeated 10:" + hist(repeat(10, q1)))
  println("repeated 1000:" + hist(repeat(1000, q1)))

  println("q2")
  val q2 = query(
    (_ : Rep[Unit]) => {
      val a = if (flip()) 1 else 0
      val b = if (flip()) 1 else 0
      val c = if (flip()) 1 else 0
      val d = a+b+c

      (a, b, c, d)
    },

    (r : Rep[(Int, Int, Int, Int)]) => r._1,

    (r: Rep[(Int, Int, Int, Int)]) => r._4 == 3)
  println(q2())
  println(hist(repeat(10, q2)))
}
