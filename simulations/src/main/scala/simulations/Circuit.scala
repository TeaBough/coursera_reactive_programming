package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " + wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {
        output.setSignal(!inputSig)
      }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) {
        output.setSignal(a1Sig & a2Sig)
      }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) {
        output.setSignal(a1Sig | a2Sig)
      }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def identityGate(a: Wire, output: Wire) {
    def identityAction() {
      val aSig = a.getSignal
      afterDelay(OrGateDelay) {
        output.setSignal(aSig)
      }
    }
    a addAction identityAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val o1 = new Wire
    val o2 = new Wire
    val o3 = new Wire
    inverter(a1, o1)
    inverter(a2, o2)
    andGate(o1, o2, o3)
    inverter(o3, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    def recurse(in: Wire, c: List[Wire], out: List[Wire]) {
      val newIn1, notControl, newIn2 = new Wire
      c match {
        case control :: rest => {
          andGate(in, control, newIn2)
          inverter(control, notControl)
          andGate(in, notControl, newIn1)

          val leftHalf = out.take(out.length / 2)
          val rightHalf = out.takeRight(out.length / 2)
          recurse(newIn2, rest, leftHalf)
          recurse(newIn1, rest, rightHalf)
        }
        case _ => {
          val w = new Wire
          w.setSignal(true)
          andGate(in, w, out.head)
        }
      }
    }
    recurse(in, c, out)
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
