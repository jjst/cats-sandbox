package sandbox

import cats.data.State

import cats.syntax.applicative._ // for pure

object StateExercise extends App {
  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = {
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }
  }

  def operator(f: (Int, Int) => Int): CalcState[Int] = State[List[Int], Int] { oldStack =>
    oldStack match {
      case op1 :: op2 :: tail => {
        val result = f(op1, op2)
        (result :: tail, result)
      }
      case _ => sys.error("Not enough operands left in stack to apply operator")
    }
  }

  def operand(num: Int): CalcState[Int] = State[List[Int], Int] { oldStack =>
    (num :: oldStack, num)
  }

  def evalAll(input: List[String]): CalcState[Int] = {
    input.foldLeft(0.pure[CalcState]) { (currState, sym) =>
      currState.flatMap { _ => evalOne(sym) }
    }
  }

  def evalInput(input: String): Int = {
    evalAll(input.split("\\s+").toList).runA(Nil).value
  }

  println(evalOne("42").runA(Nil).value)


  val program = evalAll(List("1", "2", "+", "3", "*"))

  println(program.runA(Nil).value)
}
