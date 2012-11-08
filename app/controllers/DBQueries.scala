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
  
  lazy val locations: List[Location] = DB.withConnection((conn: java.sql.Connection) => {
    val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      Jdip.locations.toList
    }
  })
  
  lazy val uniqueProvinceNames: List[UniqueProvinceName] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        Jdip.uniqueProvinceNames.toList
      }
    })
    
  lazy val empires: List[Empire] = 
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        Jdip.empires.toList
      }
    })
  
  lazy val adjacencies: List[Adjacency] = DB.withConnection((conn: java.sql.Connection) => {
    val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
    using(dbSession) {
      Jdip.adjacencies.toList
    }
  })
  
  lazy val provinces: List[Province] = 
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        Jdip.provinces.toList
      }
    })
    
  lazy val orderTypeUnitTypes: List[OrderTypeUnitType] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        Jdip.orderTypeUnitTypes.toList
      }
    })

  lazy val orderTypes: List[OrderType] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        Jdip.orderTypes.toList
      }
    })

  def getOwnedProvincesForGame(game: Game): List[OwnedProvince] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        from(Jdip.ownedProvinces) (owp =>
          where(owp.gamePlayerEmpireID in gamePlayerEmpireQueryForGame(game) and 
          	owp.gameTimeID === game.gameTime
          ) 
          select(owp)
        ).toList
      }
    })
  
  def getGamePlayerEmpire(gamePlayerEmpireID: Int): Option[GamePlayerEmpire] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        Jdip.gamePlayerEmpires.lookup(gamePlayerEmpireID)
      }
    })
    
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
  
  def getAllLandUnitsForGame(game: Game): List[DiplomacyUnit] = 
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
    	from(Jdip.diplomacyUnits)(dpu => (
          where((dpu.unitType === UnitType.ARMY) and 
              (dpu.owner in gamePlayerEmpireQueryForGame(game)) and
              (dpu.gameTime === game.gameTime)) 
          select(dpu)
        )).toList
      }
    })
  
  def getAllFleetUnitsForGame(game: Game): List[DiplomacyUnit] = 
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        from(Jdip.diplomacyUnits)(dpu => (
          where((dpu.unitType === UnitType.FLEET) and 
        	(dpu.owner in gamePlayerEmpireQueryForGame(game)) and
        	(dpu.gameTime === game.gameTime)
        ) 
        select(dpu)
      )).toList
    }
  })
  
  def getDiplomacyUnitsForGameAtCurrentGameTime(game: Game): List[DiplomacyUnit] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        from(Jdip.diplomacyUnits)(dpu => 
          where((dpu.owner in gamePlayerEmpireQueryForGame(game))
          	and (dpu.gameTime === game.gameTime))
          select(dpu)
        ).toList
      }
    })
  
  private def gamePlayerEmpireQueryForGame(game: Game): Query[Int] =
    from(Jdip.gamePlayerEmpires, Jdip.gamePlayers)((gpe, gp) => 
    	where(gpe.gamePlayerKey === gp.id and gp.gameName === game.id)
    	select(gpe.id)
    )
  
  def getGamePlayerEmpiresForGame(game: Game): List[GamePlayerEmpire] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        from(Jdip.gamePlayerEmpires) (gpe =>
          where(gpe.id in gamePlayerEmpireQueryForGame(game: Game))
          select(gpe)
        ).toList
      }
    })
  
  def getDiplomacyUnitsForGamePlayerEmpire(gamePlayerEmpire: GamePlayerEmpire):
	  List[GamePlayerEmpire] =
	    DB.withConnection((conn: java.sql.Connection) => {
	      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
	      using(dbSession) {
	        
	      }
	    })
  
  def getDiplomacyUnit(location: Location): Option[DiplomacyUnit] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        from(Jdip.diplomacyUnits)(dpu => (
          where(dpu.unitLocation === location.id) 
          select(dpu)
        )).toList.firstOption
      }
    })

  def getPlayers: List[Player] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        from(Jdip.players)(select(_)).toList
      }
    })

  def getGamePlayerEmpire(gameName: String, playerName: String): Option[GamePlayerEmpire] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        from(Jdip.gamePlayerEmpires) (gpe =>
          where(gpe.gamePlayerKey in from(Jdip.gamePlayers) (gp => 
            where(gp.gameName === gameName and gp.playerName === playerName)
            select(gp.id)))
          select(gpe)
        ).toList.headOption
      }
    })
  
  def getGamesForUser(username: String): List[Game] =
    DB.withConnection((conn: java.sql.Connection) => {
      val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
      using(dbSession) {
        from(Jdip.games) (g =>
          where(g.id in from(Jdip.gamePlayers) (gp => 
          	where (gp.playerName like username)
            select(gp.gameName)
          ))
          select(g)
        ).toList
      }
    })
    
   def getGameTimesForGames(gameTimeIDs: List[Int]): List[GameTime] =
     DB.withConnection((conn: java.sql.Connection) => {
       val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
       using(dbSession) {
         from(Jdip.gameTimes) {gt =>
           where(gt.id in gameTimeIDs)
           select(gt)
         }.toList
       }
     })
}
