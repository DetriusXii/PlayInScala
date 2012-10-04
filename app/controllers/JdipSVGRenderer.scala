package controllers

import javax.xml.parsers._
import java.io._
import org.w3c.dom._
import javax.xml.transform._
import javax.xml.transform.dom._
import javax.xml.transform.stream._

object JdipSVGRenderer {
  val SVG_FILE_PATH = "public/images/egdipmap.svg"
  val BRIEF_LABEL_LAYER_ID = "BriefLabelLayer"
  val VISIBILITY_ATTRIBUTE = "visibility"
  val VISIBLE = "visible"


  def getUneditedDocument = 
    DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(new File(SVG_FILE_PATH))

  implicit def getElementByIdOption(element: Element): Option[Element] =
    if (element != null) {
      Some(element)
    } else {
      None
    }

  def 

  def getRenderedDocument = {
    val document = getUneditedDocument

    val elementOption: Option[Element] = document.getElementById(BRIEF_LABEL_LAYER_ID)
    elementOption.map(_.setAttribute(VISIBILITY_ATTRIBUTE, VISIBLE))
    elementOption.map(_.setAttribute(
   
    println(elementOption)

    val transformer = TransformerFactory.newInstance.newTransformer
    val domSource = new DOMSource(document.getDocumentElement)

    val outputStream = new ByteArrayOutputStream()
    val result = new StreamResult(outputStream)

    transformer.transform(domSource, result)

    val byteArrayInputStream = new ByteArrayInputStream(outputStream.toByteArray)
    scala.xml.XML.load(byteArrayInputStream)
    
  }

}
