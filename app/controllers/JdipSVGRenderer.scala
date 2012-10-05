package controllers

import javax.xml.parsers._
import java.io._
import org.w3c.dom._
import javax.xml.transform._
import javax.xml.transform.dom._
import javax.xml.transform.stream._
import play.api.db.DB 
import org.squeryl.{Session => DBSession, _}

object JdipSVGRenderer {
  val SVG_FILE_PATH = "public/images/egdipmap.svg"
  val BRIEF_LABEL_LAYER_ID = "BriefLabelLayer"
  val MAP_LAYER_ID = "MapLayer"
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

  def getOwnedProvinces: List[OwnedProvince] = 
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        Jdip.ownedProvinces.toList
      }
    })

  lazy val getEmpires: List[Empire] = 
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        Jdip.empires.toList
      }
    })

  lazy val getUniqueProvinceNames: List[UniqueProvinceName] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        Jdip.uniqueProvinceNames.toList
      }
    })

  def getRenderedDocument = {
    val document = getUneditedDocument

    val briefLabelLayerOption: Option[Element] = document.getElementById(BRIEF_LABEL_LAYER_ID)
    briefLabelLayerOption.map(_.setAttribute(VISIBILITY_ATTRIBUTE, VISIBLE))
   
    
    getOwnedProvinces.map(owp => {
      game  
    })

    val transformer = TransformerFactory.newInstance.newTransformer
    val domSource = new DOMSource(document.getDocumentElement)

    val outputStream = new ByteArrayOutputStream()
    val result = new StreamResult(outputStream)

    transformer.transform(domSource, result)

    val byteArrayInputStream = new ByteArrayInputStream(outputStream.toByteArray)
    scala.xml.XML.load(byteArrayInputStream)
    
  }

}
