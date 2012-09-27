package controllers

// Checking this
import org.squeryl.{Session => DBSession, _}
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas.Jdip
import com.squeryl.jdip.tables._
import com.squeryl.jdip.adapters.RevisedPostgreSqlAdapter
import play.core.Router
import play.api.mvc._
import play.api.templates._
import play.api.db.DB
import scala.xml._
import scalaz._

class ApplicationAction[A](action: Action[A]) extends Action[A] {
  def apply(request: Request[A]): Result = action.apply(request)
  def parser: BodyParser[A] = action.parser
}

object Application extends Controller with OptionTs {
	import play.api.Play._
  
    def index = new ApplicationAction(Action {
	  Ok(views.html.Application.index())
	})
	

    def players = new ApplicationAction(Action {
      DB.withConnection((conn: java.sql.Connection) => {
        val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
        using (dbSession) {
          val players = Jdip.players
          Ok(views.html.Application.players(HeaderTitles.PLAYERS_TITLE, players))
        }
      })
      
    })
    
    def games = new ApplicationAction(Action { implicit request =>
      val username: String = session.get(Security.username) match {
        case Some(x: String) => x
        case _ => throw new Exception("No username in session")
      }
    
      DB.withConnection((conn: java.sql.Connection) => {
        val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
        using (dbSession) {
	        val gamePlayersForUser = from(Jdip.gamePlayers)((gp: GamePlayer) => 
	          where(gp.playerName === username) select(gp))
	        val gamesForUser = from(Jdip.games)((g: Game) => 
	          where(g.id in from(gamePlayersForUser)((gpu: GamePlayer) => select(gpu.gameName)))
	          select(g))
	          
	        
	        val gameTimesForUser = gamesForUser map ((g: Game) =>
	          Jdip.gameTimes.lookup(g.gameTime)
	        )
	        
	        val gamesWithGameTimes = (gamesForUser zip gameTimesForUser) filter( _._2.isDefined) map(u =>
	        	(u._1, u._2.get)
	        )
	          
	        val gameScreenURL = controllers.routes.Application.gameScreen("").url
	          
	        Ok(views.html.Application.games(gameScreenURL, gamesWithGameTimes))
        }
      })
    })

    implicit def toOptionTFromOption[A](option: Option[A]): OptionT[List, A] = optionT[List](option :: Nil)

    def gameScreen(gameName: String = "") = new ApplicationAction(Action { implicit request =>
      import Scalaz._
      
      val usernameValidation: Validation[Exception, String] = 
        session.get(Security.username).toSuccess(new Exception("No username in session"))
      
      DB.withConnection((conn: java.sql.Connection) => {
        val dbSession = DBSession.create(conn, new RevisedPostgreSqlAdapter)
        using(dbSession) {
          
          def getGamePlayer(game: Game, player: Player): Validation[Exception, GamePlayer] = 
            from(Jdip.gamePlayers)((gp: GamePlayer) =>
            	where(gp.gameName === game.id and gp.playerName === player.id)
            	select(gp)
            ).firstOption.toSuccess(new Exception(
                "No gamePlayer found with game name %s and player name %s" format (game.id, player.id)    
            ))
            
          def getGamePlayerEmpire(gamePlayer: GamePlayer): Validation[Exception, GamePlayerEmpire] =
            from(Jdip.gamePlayerEmpires)((gpe: GamePlayerEmpire) => 
            	where(gpe.gamePlayerKey === gamePlayer.id)
            	select(gpe)
            ).firstOption.toSuccess(new Exception(
                "No GamePlayerEmpire found with GamePlayer ID %d" format gamePlayer.id
            ))
            
          def getDiplomacyUnits(gamePlayerEmpire: GamePlayerEmpire): Iterable[DiplomacyUnit] =
            from(Jdip.diplomacyUnits)((dpu: DiplomacyUnit) =>
              where(dpu.owner === gamePlayerEmpire.id)
              select(dpu)
            )
            
          val diplomacyUnitsValidation = for ( 
                username <- usernameValidation;
                game <- Jdip.games.lookup(gameName).
        		      toSuccess(new Exception("No game found with game name %s" format gameName));
                player <- Jdip.players.lookup(username).
        		      toSuccess(new Exception("No player found with player name %s" format username));
                gamePlayer <- getGamePlayer(game, player);
        	      gamePlayerEmpire <- getGamePlayerEmpire(gamePlayer)
              ) yield ( getDiplomacyUnits(gamePlayerEmpire) )
         
          
          diplomacyUnitsValidation match {
            case Success(diplomacyUnits: Iterable[_]) => {
              val actions = from(Jdip.orderTypeUnitTypes)((otut: OrderTypeUnitType) => 
            	  where(otut.orderType in from(Jdip.orderTypes)((ot: OrderType) => 
                  where(ot.phase === Phase.MOVEMENT) select(ot.id)))	  
                select(otut))
              val armyActions = actions.filter((otut: OrderTypeUnitType) => 
                otut.unitType.equals(UnitType.ARMY))
              val fleetActions = actions.filter((otut: OrderTypeUnitType) => 
                otut.unitType.equals(UnitType.FLEET))
              
              val moveOrdersMap = CommonQueries.getMoveOrdersMap(diplomacyUnits)
              val supportHoldsMap = CommonQueries.getSupportHoldsMap(diplomacyUnits)
              val supportMovesMap = CommonQueries.getSupportMovesMap(diplomacyUnits)              

              println(supportHoldsMap);

              Ok(views.html.Application.gameScreen(getGameScreenData(diplomacyUnits),
                  moveOrdersMap,
                  supportHoldsMap,
                  fleetActions, 
                  armyActions))
            }
            case Failure(e: Exception) => Ok(e.getMessage)
          }
        }
      })
    })
	
  

	private def getGameScreenData(diplomacyUnits: Iterable[DiplomacyUnit]): 
    Iterable[Tuple2[String, String]] =
    diplomacyUnits.map((dpu: DiplomacyUnit) =>
      Jdip.
        locations.
        lookup(dpu.unitLocation).
        map(CommonQueries.getFormattedLocationName(_)).
        map((dpu.unitType, _))       
    ).flatten
            
      
	  
}
