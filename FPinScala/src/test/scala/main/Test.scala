package main

import main.chapter2.Chapter2
import org.scalatest.{Matchers, FlatSpec}

/**
  * Created by adrian on 3/8/2016.
  */
class Test extends FlatSpec with Matchers {

  trait General
  case object Specific1 extends  General
  case object Specific2 extends  General


  "case class" should "match" in {
    val spec1 = Specific1

    val genSpec1: General = spec1.asInstanceOf[General]

    genSpec1 match {
      case Specific1 => println(Specific1)
      case Specific2 => println(Specific2)
      case _ => println("not specific 1 or 2")
    }
  }


}
