/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package controllers

import play.mvc._
import scala.xml._
import play.templates._

object UnitTypes extends Controller {
  val armySymbol = 
    <symbol id="Army" viewBox="0 0 23 15" overflow="visible">
            <g>
              <rect x="2" y="2" width="23" height="13" rx="4" stroke-width="1" class="symShadow" />
              <rect x="0" y="0" width="23" height="13" rx="4" class="symBorder" />
              <g class="symSilhouette">
                      <rect x="6" y="6" width="13" height="1"/>
                      <rect x="5" y="7" width="14" height="1"/>
                      <rect x="6" y="8" width="12" height="1"/>
                      <rect x="7" y="9" width="10" height="1"/>
                      <rect x="10" y="3" width="5" height="3"/>
                      <rect x="15" y="4.5" width="1" height="1.5"/>
                      <line x1="3" y1="4" x2="10" y2="4"/>
              </g>
            </g>	
    </symbol>

  val fleetSymbol = <symbol id="Fleet" viewBox="0 0 23 15" overflow="visible">
			<g>
                          <rect x="2" y="2" width="23" height="13" rx="4" 
                                stroke-width="1" class="symShadow" />
                          <rect x="0" y="0" width="23" height="13" rx="4" class="symBorder" />
                          <g class="symSilhouette">
                            <rect x="3" y="7" width="16.5" height="1"/>
                            <rect x="4" y="8" width="15" height="1"/>
                            <rect x="5" y="9" width="13.5" height="1"/>
                            <rect x="13.5" y="6" width="2.75" height="1"/>
                            <rect x="7" y="5"  width="4" height="2"/>
                            <rect x="8.5" y="4"  width="1" height="1"/>
                            <rect x="6" y="6" width="1" height="1"/>
                          </g>
			</g>	
                    </symbol>
  
  def getSVGTemplate(x: Elem): Html = {
    val id = x.attribute("id") match {
      case Some(x) => x.toString
      case _ => throw new Exception("ID attribute not found")
    }
    
    val anchor = "#" + id
    
    HtmlFormat.raw(<svg xmlns="http://www.w3.org/2000/svg" 
        xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 100 100">
        <defs>
          {x}
        </defs>
      <g>
        <use fill="deepskyblue" xlink:href={anchor} />
      </g>
    </svg>.toString)
  } 
  
  val armyType = getSVGTemplate(armySymbol)
  val fleetType = getSVGTemplate(fleetSymbol)
}
