package edu.berkeley.crea.io

import it.uniroma1.dis.wsngroup.gexf4j.core.viz.Color
import it.uniroma1.dis.wsngroup.gexf4j.core.impl.viz.ColorImpl

/** 
  * @object Colors - contains utility methods for converting  
  * colors to / from hexadecimal number representation. Used 
  * for importing / exporting with database.
 **/
object Colors { 

  /** 
    * @method hex 
    * @param c {Color} 
    * @return {Int} 
   **/
  def hex(c : Color) : Int = { 
    (c.getR << 16) + (c.getG << 8) + c.getB 
  }
 
  /**
    * @method hex 
    * @param c {Int} 
    * @return {Color} 
   **/
  def obj(c : Int) : Color = {

    val r : Int = (c >> 16) & 0xFF
    val g : Int = (c >> 8) & 0xFF
    val b : Int = c & 0xFF 

    new ColorImpl(r, g, b) 

  } 

} 
