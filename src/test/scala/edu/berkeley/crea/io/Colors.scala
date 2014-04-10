package edu.berkeley.crea.io


import it.uniroma1.dis.wsngroup.gexf4j.core.viz.Color
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.viz.ColorImpl

import org.scalatest._

class ColorsSpec extends FlatSpec with Matchers {

  "Colors" should "convert colors between objects and hexadecimal numbers." in { 

    val hexForm : Int = 0x45E4F3
    val objForm : Color =  Colors.obj(hexForm)

    assert(hexForm == Colors.hex(objForm))

  } 
} 
