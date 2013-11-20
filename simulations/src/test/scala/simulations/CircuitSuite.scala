package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.junit.Ignore

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

 /*
  test("demux with 2 controls test") {
    val in, c0, c1, out1, out2,  out3, out4  = new Wire
    demux(in, List(c1, c0), List(out4, out3, out2, out1))
    in.setSignal(true)
    c0.setSignal(false)
    c1.setSignal(true)
    run
    println(List(out4, out3, out2, out1).map(_.getSignal))
    assert(out1.getSignal === false, "out1 signal")
    assert(out2.getSignal === false, "out2 signal")
    assert(out3.getSignal === true, "out3 signal")
    assert(out4.getSignal === false, "out4 signal")
  } */

 /* test("1") {
    val in, out = new Wire
    demux(in, Nil, List(out))
    in.setSignal(false)
    run

    assert(out.getSignal == false, "demux0 1")

    in.setSignal(true)
    run

    assert(out.getSignal == true, "demux0 2")
  }


  test("2") {
    val in, c, o0, o1 = new Wire
    demux(in, List(c), List(o1,o0))
    in.setSignal(false)
    c.setSignal(false)
    run

    assert(o0.getSignal == false, "demux1 1")
    assert(o1.getSignal == false, "demux1 1")

    in.setSignal(true)
    run
    println(List(o1,o0).map(_.getSignal))
    assert(o0.getSignal == true, "demux1 2")
    assert(o1.getSignal == false, "demux1 2")


    c.setSignal(true)
    run

    assert(o0.getSignal == false, "demux1 3")
    assert(o1.getSignal == true, "demux1 3")

    in.setSignal(false)
    run

    assert(o0.getSignal == false, "demux1 4")
    assert(o1.getSignal == false, "demux1 4")
  }

  test("3"){
    val in, c0, c1, o0, o1, o2, o3 = new Wire
    demux(in, List(c1,c0), List(o3,o2,o1,o0))
    in.setSignal(false)
    c0.setSignal(false)
    c1.setSignal(false)
    run

    assert(o0.getSignal == false, "demux2 1")
    assert(o1.getSignal == false, "demux2 1")
    assert(o2.getSignal == false, "demux2 1")
    assert(o3.getSignal == false, "demux2 1")

    in.setSignal(true)
    run

    assert(o0.getSignal == true, "demux2 2")
    assert(o1.getSignal == false, "demux2 2")
    assert(o2.getSignal == false, "demux2 2")
    assert(o3.getSignal == false, "demux2 2")


    c1.setSignal(true)
    run
    println(List(o3,o2,o1,o0).map(_.getSignal))
    assert(o0.getSignal == false, "demux2 3")
    assert(o1.getSignal == false, "demux2 3")
    assert(o2.getSignal == true, "demux2 3")
    assert(o3.getSignal == false, "demux2 3")

    in.setSignal(false)
    run

    assert(o0.getSignal == false, "demux2 4")
    assert(o1.getSignal == false, "demux2 4")
    assert(o2.getSignal == false, "demux2 4")
    assert(o3.getSignal == false, "demux2 4")
  }
*/
  test("demux with 2 controls test") {
    val in, c0, c1, out1, out2,  out3, out4  = new Wire
    demux(in, List(c1, c0), List(out4, out3, out2, out1))
    in.setSignal(true)
    c0.setSignal(false)
    c1.setSignal(true)
    run
    println("A TOI : " + List(out4, out3, out2, out1).reverse.map(_.getSignal))
    assert(out1.getSignal === false, "out1 signal")
    assert(out2.getSignal === false, "out2 signal")
    assert(out3.getSignal === true, "out3 signal")
    assert(out4.getSignal === false, "out4 signal")
  }

  test("demux medium") {
    val in, c0, c1, o0, o1, o2, o3 = new Wire
    val c = c1 :: c0 :: Nil
    val o = o3 :: o2 :: o1 :: o0 :: Nil
    demux(in, c, o)

    run
    assert(o0.getSignal === false, 00)
    assert(o1.getSignal === false, 11)
    assert(o2.getSignal === false, 22)
    assert(o3.getSignal === false, 33)

    in.setSignal(true)
    c1.setSignal(false)
    c0.setSignal(false)

    run
    println(o.map(_.getSignal))
    assert(o0.getSignal === true, 10)
    assert(o1.getSignal === false, 21)
    assert(o2.getSignal === false, 32)
    assert(o3.getSignal === false, 43)

    in.setSignal(true)
    c1.setSignal(false)
    c0.setSignal(true)
    run
    println(o.map(_.getSignal))
    assert(o0.getSignal === false, 000)
    assert(o1.getSignal === true, 100)
    assert(o2.getSignal === false, 200)
    assert(o3.getSignal === false, 300)
  }

  test("demux large") {
    val in, c0, c1, c2, c3, o0, o1, o2, o3, o4, o5, o6, o7, o8, o9, o10, o11, o12, o13, o14, o15 = new Wire
    val c = c3 :: c2 :: c1 :: c0 :: Nil
    val o = o15 :: o14 :: o13 :: o12 :: o11 :: o10 :: o9 :: o8 :: o7 :: o6 :: o5 :: o4 :: o3 :: o2 :: o1 :: o0 :: Nil
    demux(in, c, o)

    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)
    assert(o4.getSignal === false, "o4")
    assert(o5.getSignal === false, "o5")
    assert(o6.getSignal === false, "o6")
    assert(o7.getSignal === false, "o7")
    assert(o8.getSignal === false, "o8")
    assert(o9.getSignal === false, "o9")
    assert(o10.getSignal === false, "o10")
    assert(o11.getSignal === false, "o11")
    assert(o12.getSignal === false, "o12")
    assert(o13.getSignal === false, "o13")
    assert(o14.getSignal === false, "o14")
    assert(o15.getSignal === false, "o15")

    in.setSignal(true)
    run
    assert(o0.getSignal === true, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)
    assert(o4.getSignal === false, "o4")
    assert(o5.getSignal === false, "o5")
    assert(o6.getSignal === false, "o6")
    assert(o7.getSignal === false, "o7")
    assert(o8.getSignal === false, "o8")
    assert(o9.getSignal === false, "o9")
    assert(o10.getSignal === false, "o10")
    assert(o11.getSignal === false, "o11")
    assert(o12.getSignal === false, "o12")
    assert(o13.getSignal === false, "o13")
    assert(o14.getSignal === false, "o14")
    assert(o15.getSignal === false, "o15")

    in.setSignal(true)
    c0.setSignal(true)
    c3.setSignal(true)
    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)
    assert(o4.getSignal === false, "o4")
    assert(o5.getSignal === false, "o5")
    assert(o6.getSignal === false, "o6")
    assert(o7.getSignal === false, "o7")
    assert(o8.getSignal === false, "o8")
    assert(o9.getSignal === true, "o9")
    assert(o10.getSignal === false, "o10")
    assert(o11.getSignal === false, "o11")
    assert(o12.getSignal === false, "o12")
    assert(o13.getSignal === false, "o13")
    assert(o14.getSignal === false, "o14")
    assert(o15.getSignal === false, "o15")
  }


}
