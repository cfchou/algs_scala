/**
 * Created with IntelliJ IDEA.
 * User: cfchou
 * Date: 02/07/2013
 */

// evaluate expressions like: ( 1 + ( 2 + 3 ) * ( 4 * 5 ) ) )
object eval extends App {

  type BiOp = Function2[Int, Int, Int]

  object Op {
    def unapply (s: String): Option[BiOp] = {
      s match {
        case "+" =>
          Some(new BiOp {
            def apply(v1: Int, v2: Int): Int = v1 + v2
          })
        case "-" =>
          Some(new BiOp {
            def apply(v1: Int, v2: Int): Int = v1 - v2
          })
        case "*" =>
          Some(new BiOp {
            def apply(v1: Int, v2: Int): Int = v1 * v2
          })
        case "/" =>
          Some(new BiOp {
            def apply(v1: Int, v2: Int): Int = v1 / v2
          })
        case _ => None
      }
    }
  }
  object Num {
    def unapply (s: String): Option[Int] =
      if (s matches "[+-]?\\d+") Some(s toInt)
      else None
  }

  val s = Console.readLine()
  val syms = s.split(' ')

  val (ostk, istk) = syms.foldLeft (new EStack[BiOp], new EStack[Int]) { (stks, s) =>
    val ops = stks._1
    val ints = stks._2
    s match {
      case Op(f) => ops.push(f)
      case Num(i) => ints.push(i)
      case ")" => {
        val rv = ints.pop
        val op = ops.pop
        val lv = ints.pop
        ints.push(op(lv, rv))
      }
      case _ =>
    }
    stks
  }

  if (istk.isEmpty)
    Console.println("No input")
  else
    Console.println(istk.pop)
}
