package controllers

import org.apache.commons.mail.SimpleEmail
import play._
import play.libs.Mail
import play.mvc._
import play.mvc.results.Redirect
import play.mvc.results.RenderHtml
import scala.actors.remote.RemoteActor
import scala.actors.remote.Node
import scala.actors.Actor

import scala.util.parsing.json._

object Application extends Controller {

    def index = Template

    def login_credentials(username : String, password : String ) = {
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
    }
    
}
