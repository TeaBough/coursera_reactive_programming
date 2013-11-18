package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1,in2,out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1,in2,out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  test("demux example") {
    val in = new Wire
    val c1, c2, c3 = new Wire
    val out1, out2, out3, out4, out5, out6, out7, out8 = new Wire
    val c = List(c1)
    val out = List(out1, out2)
    demux(in,c,out)

    in.setSignal(true)
    c1.setSignal(true)
    run

    println(out.map(_.getSignal))
    assert(out.head.getSignal === true)
  }

   test("demux1 example") {
    val in = new Wire
    val c0 = new Wire
    val out0 = new Wire
    val out1 = new Wire
    demux(in, List(c0), List(out1, out0))

    c0.setSignal(false)
    in.setSignal(false)
    run
    assert(out0.getSignal === false, "demux1 1")
    assert(out1.getSignal === false, "demux1 2")

    in.setSignal(true)
    run
    assert(out0.getSignal === true, "demux1 3")
    assert(out1.getSignal === false, "demux1 4")

    c0.setSignal(true)
    run
    assert(out0.getSignal === false, "demux1 5")
    assert(out1.getSignal === true, "demux1 6")
  }


  test("demux with 2 controls test") {
    val in, c0, c1, out1, out2,  out3, out4  = new Wire
    demux(in, List(c1, c0), List(out4, out3, out2, out1))
    in.setSignal(true)
    c0.setSignal(false)
    c1.setSignal(true)
    run

    assert(out1.getSignal === false, "out1 signal")
    assert(out2.getSignal === false, "out2 signal")
    assert(out3.getSignal === true, "out3 signal")
    assert(out4.getSignal === false, "out4 signal")
  }


  //
  // to complete with tests for orGate, demux, ...
  //

}
