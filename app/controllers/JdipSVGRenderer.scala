package controllers

import javax.xml.parsers._
import java.io._
import org.w3c.dom._
import javax.xml.transform._
import javax.xml.transform.dom._
import javax.xml.transform.stream._
import play.api.db.DB
import org.squeryl.{Session => DBSession, _}
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.tables._
import com.squeryl.jdip.adapters._
import com.squeryl.jdip.schemas._

object JdipSVGRenderer {
  import play.api.Play._
  
  val SVG_FILE_PATH = "public/images/dipmapwithsymbols.svg"
  val BRIEF_LABEL_LAYER_ID = "BriefLabelLayer"
  val MAP_LAYER_ID = "MapLayer"
  val UNIT_LAYER_ID = "UnitLayer"
  val VISIBILITY_ATTRIBUTE = "visibility"
  val VISIBLE = "visible"
  val CLASS_ATTRIBUTE = "class"
  val USE_LABEL = "use"
  val JDIP_NAMESPACE = "jdipNS"
  val PROVINCE_LABEL = "PROVINCE"
  val NAME_ATTRIBUTE = "name"
  val UNIT_LABEL = "UNIT"
  val X_ATTRIBUTE = "x"
  val Y_ATTRIBUTE = "y"
  val XLINK_NAMESPACE = "xlink"
  val HREF_ATTRIBUTE = "href"
  val FILL_ATTRIBUTE = "fill"
  val HEIGHT_VALUE = "30"
  val WIDTH_VALUE = "30"
  val HEIGHT_ATTRIBUTE = "height"
  val WIDTH_ATTRIBUTE = "width"
  val unitTypeRemap = Map(UnitType.ARMY -> "Army", UnitType.FLEET -> "Fleet")
  


  def getUneditedDocument = 
    DocumentBuilderFactory.newInstance.newDocumentBuilder.parse(new File(SVG_FILE_PATH))

  implicit def getElementByIdOption(element: Element): Option[Element] =
    if (element != null) {
      Some(element)
    } else {
      None
    }
  
  implicit def getScalaNodeList(nodeList: NodeList): List[Element] =
    (0 until nodeList.getLength()).toList.map(nodeList.item(_)).filter((u: Node) =>
      u.isInstanceOf[Element]
    ).map(_ match {
      case x: Element => x
    })   

  def addProvinceColours(document: Document)(game: Game): Unit = {
    DBQueries.getOwnedProvincesForGame(game).map((owp: OwnedProvince) => {
      val province = owp.province
      val gamePlayerEmpireID = owp.gamePlayerEmpireID
      
      val uniqueProvinceNamesForProvince = 
        DBQueries.uniqueProvinceNames.filter(_.provinceName.equals(province))
      
      val gamePlayerEmpireOption = DBQueries.getGamePlayerEmpire(gamePlayerEmpireID)
      val provinceColourOption = gamePlayerEmpireOption.map(_ match {
        case GamePlayerEmpire(_, empire) => empire
      }).flatMap((empireName: String) => {
        DBQueries.empires.find(_.id.equals(empireName)).map(_.provinceColour)
      })
      
      uniqueProvinceNamesForProvince.map((upn: UniqueProvinceName) => {
        val mapLayerElementOption: Option[Element] = 
          document.getElementById("_%s" format upn.alternateName)
        mapLayerElementOption.flatMap(element =>
          provinceColourOption.map(element.setAttribute(CLASS_ATTRIBUTE, _))
        )
      })
    })
  }
  
  def getRenderedDocument(game: Game) = {
    val document = getUneditedDocument

    val briefLabelLayerOption: Option[Element] = 
      document.getElementById(BRIEF_LABEL_LAYER_ID)
    briefLabelLayerOption.map(_.setAttribute(VISIBILITY_ATTRIBUTE, VISIBLE))
   
    addProvinceColours(document)(game)
    addUnitsToDocument(document)(game)

    val transformer = TransformerFactory.newInstance.newTransformer
    val domSource = new DOMSource(document.getDocumentElement)

    val outputStream = new ByteArrayOutputStream()
    val result = new StreamResult(outputStream)

    transformer.transform(domSource, result)

    val byteArrayInputStream = new ByteArrayInputStream(outputStream.toByteArray)
    scala.xml.XML.load(byteArrayInputStream)
    
  }

  def addUnitsToDocument(document: Document)(game: Game): Unit = {
    val diplomacyUnits = DBQueries.getDiplomacyUnitsForGameAtCurrentGameTime(game)
    val unitLayer: Option[Element] = document.getElementById(UNIT_LAYER_ID)
    val fullProvinceElementList: List[Element] = 
      document.getElementsByTagName("%s:%s" format (JDIP_NAMESPACE, PROVINCE_LABEL))
    
    diplomacyUnits.map(dpu => {
      val locationOption = DBQueries.locations.find(_.id == dpu.unitLocation)
      
      val gamePlayerEmpireOption = DBQueries.getGamePlayerEmpire(dpu.owner)
      val empireOption = gamePlayerEmpireOption.flatMap(gpe => 
        DBQueries.empires.find(_.id == gpe.empireName))
      
      val uniqueProvinceNamesOption = locationOption.map(_ match {
        case Location(province, coast) => 
          (DBQueries.uniqueProvinceNames.filter(_.provinceName.equals(province)), coast)
      }).map((u: Tuple2[List[UniqueProvinceName], String]) => {
        val coast = u._2
        val uniqueProvinceNames = u._1
        
        
        val provinceElementList = fullProvinceElementList.filter(provElement => {
            coast match {
              case Coast.ALL_COAST | Coast.NO_COAST => uniqueProvinceNames.exists(
                _.alternateName.equals(provElement.getAttribute(NAME_ATTRIBUTE))
              )
              case _ => uniqueProvinceNames.exists(upn =>
            	("%s-%s" format (upn.alternateName, coast)).
            		equals(provElement.getAttribute(NAME_ATTRIBUTE))
              )
            }
          })
        
        val unitElements = provinceElementList.map(element => {
          val unitElements: List[Element] = 
            element.getElementsByTagName("%s:%s" format (JDIP_NAMESPACE, UNIT_LABEL))
          unitElements
        }).flatten
        
        
        unitElements.foreach(unitElement => {
          val x = unitElement.getAttribute(X_ATTRIBUTE)
          val y = unitElement.getAttribute(Y_ATTRIBUTE)
          
          val useElement = document.createElement(USE_LABEL)
          useElement.setAttribute(X_ATTRIBUTE, x)
          useElement.setAttribute(Y_ATTRIBUTE, y)
          val unitTypeOption = unitTypeRemap.get(dpu.unitType)
          unitTypeOption.map(unitType =>
              useElement.setAttribute("%s:%s" format (XLINK_NAMESPACE, HREF_ATTRIBUTE), 
                  "#%s" format unitType))
          useElement.setAttribute(HEIGHT_ATTRIBUTE, HEIGHT_VALUE)
          useElement.setAttribute(WIDTH_ATTRIBUTE, WIDTH_VALUE)
          empireOption.map(empire => 
            useElement.setAttribute(CLASS_ATTRIBUTE, empire.unitColour))
          
          unitLayer.map(_.appendChild(useElement))
        })
      })
      
      
    })
  }
}
