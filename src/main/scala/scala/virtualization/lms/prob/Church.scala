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

trait ChurchLangBase extends ChurchBase with IfThenElse with NumericOps with ListOps with TupleOps
trait ChurchLangExp extends ChurchBaseExp with IfThenElseExp with NumericOpsExp with ListOpsExp with TupleOpsExp
trait ScalaGenChurchLang extends ScalaGenChurchBase with ScalaGenIfThenElse with ScalaGenNumericOps with ScalaGenListOps with ScalaGenTupleOps {
  val IR: ChurchLangExp

  override def utilRandom: String = "(new scala.util.Random())"
}
trait ChurchLang extends ChurchLangExp with CompileScala { q =>
  object codegen extends ScalaGenChurchLang {
    val IR: q.type = q
  }
  def query[P:Manifest,R:Manifest](program: Rep[Unit] => Rep[P], result: Rep[P] => Rep[R], predicate: Rep[P] => Rep[Boolean]): R = {
    val f_program = compile(program)
    val f_result = compile(result)
    val f_predicate = compile(predicate)

    var p = f_program()
    while (!f_predicate(p)) {
      println("discarding " + p)
      p = f_program()
    }
    println("accepting " + p)
    f_result(p)
  }
}

object TestChurch extends App with ChurchLang {
  println("q1")
  println(query(
    (_ : Rep[Unit]) => flip(),
    (x: Rep[Boolean]) => x,
    (x: Rep[Boolean]) => unit(true)))
}
