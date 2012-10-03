package controllers

import javax.xml.parsers._
import org.w3c.dom._

object JdipSVGRenderer {
  val SVG_FILE_PATH = "public/images/egdipmap.svg"
  val BRIEF_LABEL_LAYER_ID = "BriefLabelLayer"
  val VISIBILITY_ATTRIBUTE = "visibility"
  val VISIBLE = "visible"


  def getUneditedDocument = 
    DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(new File(SVG_FILE_PATH))

  implicit def getElementByIdOption(element: Element): Option[Element] =
    if (element != null) {
      None
    } else {
      Some(element)
    }

  def getRenderedDocument = {
    val document = getUneditedDocument

    val elementOption: Option[Element] = document.getElementById(BRIEF_LABEL_LAYER_ID)
    elementOption.map(_.setAttribute(VISIBILITY_ATTRIBUTE, VISIBLE))
    
    
  }

}
