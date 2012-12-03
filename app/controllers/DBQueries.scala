package controllers

import scalaz._
import scalaz.effects._
import play.api.db.DB
import org.squeryl.{Session => DBSession, _}
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas._
import com.squeryl.jdip.tables._
import com.squeryl.jdip.adapters._

object DBQueries extends States  {
  import play.api.Play._
  
  lazy val dbQueries: com.squeryl.jdip.queries.DBQueries = 
    DB.withConnection((conn: java.sql.Connection) => {
      new com.squeryl.jdip.queries.DBQueries(conn)
    })
  
  lazy val locations: List[Location] = dbQueries.locations
  
  lazy val adjacencies: List[Adjacency] = dbQueries.adjacencies
  
  lazy val provinces: List[Province] = dbQueries.provinces
    
  lazy val orderTypeUnitTypes: List[OrderTypeUnitType] =
    dbQueries.orderTypeUnitTypes

  lazy val orderTypes: List[OrderType] =
    dbQueries.orderTypes


  def getAdjacentLocationsForLocation(loc: Location): List[Location] = {
    adjacencies.filter(_.srcLocation == loc.id).map(adj => {
      locations.find(_.id == adj.dstLocation)
    }).flatten
  }
  
  def getCoastLocationsFromLandLocation(loc: Location): List[Location] = {
    locations.filter(_ match {
      case Location(loc.province, Coast.NO_COAST) => false
      case Location(loc.province, _) => true
      case _ => false
    })
  }
  
  def hasLandLocation(loc: Location): Boolean =
    locations.exists(_ match {
      case Location(loc.province, Coast.NO_COAST) => true
      case _ => false
    })
  
  def getPlayers: List[Player] = dbQueries.getPlayers
}
