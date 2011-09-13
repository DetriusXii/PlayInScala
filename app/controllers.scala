package controllers


import org.squeryl._
import org.squeryl.dsl._
import org.squeryl.PrimitiveTypeMode._
import com.squeryl.jdip.schemas.Jdip
import com.squeryl.jdip.tables.Players
import com.squeryl.jdip.tables.Empires
import com.squeryl.jdip.adapters.PostgreSqlAdapter
import play.mvc.Controller

import play.db.DB

object Application extends Controller {
  
    def index = views.Application.html.index()

    def players = {
      val session = Session.create(DB.getConnection, new PostgreSqlAdapter)
      using(session) {
        val players = Jdip.players
        val playersListItem = players map ((u: Players) => <li>{u.id}</li>)
        <html>
          <body>
            <ul>
              {playersListItem}
            </ul>
          </body>
        </html>
      }
    }
    
    def games = {
      val session = Session.create(DB.getConnection, new PostgreSqlAdapter)
      using(session) {
        val games = Jdip.games
        views.Application.html.games(games)
      }
    }
  
    def empires  = {
      val session = Session.create(DB.getConnection, new PostgreSqlAdapter)
      using(session) {
        val empires: Iterable[Empires] = Jdip.empires map ((u: Empires) => u)
        views.Application.html.empires(empires)
      }
    }
  
    /*def login_credentials(username : String, password : String ) = {
      Redirect("/redirected_page?username=" + username) 
    }

    def emailTester(emailAddress : String = "", testMessage : String = "") {
      val x = emailAddress match {
        case "" => "No email address entered"
        case x : String => {
            "We sent you an email to the following address: " + emailAddress
        }
      }

      val simpleEmail : SimpleEmail = new SimpleEmail
      simpleEmail.setFrom("ajelovic@idnoodle.com")
      simpleEmail.addTo("ajelovic@yahoo.com")
      simpleEmail.setSubject("Welcome to the Diplomacy website")
      simpleEmail.setMsg("Here is your test Message " + testMessage)
      Mail.send(simpleEmail)

      x
    }

    def redirected_page(username : String = "") = {
      val serverActor = RemoteActor.select(Node("localhost", 5401), Symbol("myname"))
      val unitCommandList = (serverActor !? Some((username, "getMovements"))).asInstanceOf[List[Map[String, String]]]

      val formattedUnitCommandList = unitCommandList map ((u) => {
        val Some(unitType) = u.get("unitType")
        val Some(provinceName) = u.get("provinceName")
        val Some(orderType) = u.get("orderType")
        val Some(srcName) = u.get("srcName")
        val Some(dstName) = u.get("dstName")
        String.format("{unitType: '%s', provinceName: '%s', orderType: '%s', srcName: '%s', dstName: '%s'}",
                      unitType,
                      provinceName,
                      orderType,
                      srcName,
                      dstName);
      })

      val unitList = unitCommandList.foldLeft(Nil : List[Map[String,String]])((u,v) => {
          val Some(unitType) = v.get("unitType")
          val Some(provinceName) = v.get("provinceName")
          val x = Map("unitType" -> unitType, "provinceName" -> provinceName)
          if (u.contains(x))
            u
          else
            x :: u
      })

      val formattedUnitList = unitList map (u => {
        val Some(unitType) = u.get("unitType")
        val Some(provinceName) = u.get("provinceName")
        String.format("{unitType: '%s', provinceName: '%s'}", unitType, provinceName)
      })

      val unitCommands = String.format("[%s]", formattedUnitCommandList.mkString(","))
      val units = String.format("[%s]", formattedUnitList.mkString(","))
      Template(("unitCommands", unitCommands), ("units", units))
    }*/
    
}
