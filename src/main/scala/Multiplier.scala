// See LICENSE for license details.
import chisel3._
import chisel3.util.{Cat, Fill}

trait MultIO { self: Multiplier =>
  val inp = IO(Input(new Bundle{
    val a = UInt(width.W)
    val b = UInt(width.W)
  }))
  val res = IO(Output(UInt((2 * width).W)))
}

sealed trait MultType
case object MultUU extends MultType
case object MultUS extends MultType
case object MultSS extends MultType


class Multiplier(val width: Int, mtype: MultType) extends RawModule with MultIO {

  override val desiredName = mtype.getClass.getSimpleName + width

  val s = width
  val t = s >> 1

  res := (t match {
    case 0 => {
      mtype match {
        case MultUS => Fill(2, inp.a & inp.b)
        case _ => inp.a & inp.b
      }
    }
    case _ => {
      val q0 = Multiplier(inp.a(t - 1, 0), inp.b(t - 1, 0), MultUU, Some("albl"))

      val q1 = mtype match {
        case MultUU => Multiplier(inp.a(t - 1, 0), inp.b(s - 1, t), MultUU, Some("albh"))
        case _ => Multiplier(inp.a(t - 1, 0), inp.b(s - 1, t), MultUS, Some("albh"))
      }

      val q2 = mtype match {
        case MultSS => Multiplier(inp.b(t - 1, 0), inp.a(s - 1, t), MultUS, Some("ahbl"))
        case _ => Multiplier(inp.b(t - 1, 0), inp.a(s - 1, t), MultUU, Some("ahbl"))
      }

      val q3 = Multiplier(inp.a(s - 1, t), inp.b(s - 1, t), mtype, Some("ahbh"))

      val middle = (mtype match {
        case MultUU => Cat(0.U(1.W), q1) + Cat(0.U(1.W), q2)
        case MultUS => Cat(q1(s - 1), q1) + Cat(0.U(1.W), q2)
        case MultSS => Cat(q1(s - 1), q1) + Cat(q2(s - 1), q2)
      })

      Cat(q3, q0) + Cat(Fill(t, middle(s)), middle, 0.U(t.W))
    }
  })
}

object Multiplier{
  def apply(a: UInt, b: UInt, mtype: MultType = MultUU, name: Option[String] = None): UInt = {
    val width = Seq(a.getWidth, b.getWidth) max
    val m = Module(new Multiplier(width, mtype))
    name foreach {m.suggestName(_)}
    m.inp.a := a
    m.inp.b := b
    m.res
  }
}