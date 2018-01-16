package com.arulselvan.caltrain.schedule.parser

import cats.instances.string._
import cats.syntax.semigroup._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
// import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.Json

final case class TrainInfo(station: String,
                           zone: String,
                           northbound: Iterable[String],
                           southbound: Iterable[String])

object Main {
  val CALTRAIN_SITE = "http://www.caltrain.com/schedules/weekdaytimetable.html"

  def constructJsonFormat(nbTrain:Element, sbTrain:Element):Json = {
    val stationName:String = nbTrain >> text("th a")
    val zoneId:String = nbTrain >> text("th")
    val toNorth:Iterable[String] = (nbTrain >> texts("td")).filterNot(_.isEmpty)
    val toSouth:Iterable[String] = (sbTrain >> texts("td")).filterNot(_.isEmpty)
    // println(s"${stationName}\t${zoneId}\t${toNorth.length}\t${toSouth.length}")
    TrainInfo(stationName, zoneId, toNorth, toSouth).asJson
  }

  def main(args: Array[String]): Unit = {
    println("Hello " |+| "Cats!")
    val browser: Browser = JsoupBrowser()
    val doc = browser.get(CALTRAIN_SITE)
    val northboundTT: Element = doc >> element(".NB_TT")
    val southboundTT: Element = doc >> element(".SB_TT")
    val nbTrains = northboundTT >> elementList("tbody tr")
    val sbTrains = (southboundTT >> elementList("tbody tr")).reverse
    val allTrains = (nbTrains zip sbTrains)
    val allStations = for {
      (nbTrain, sbTrain) <- allTrains
    } yield constructJsonFormat(nbTrain, sbTrain)
    println(allStations)
    // for {
    //   table <- northboundTT
    //   thead = table >> element("thead")
    //   tbody = table >> element("tbody")
    //   tr <- tbody >> elementList("tr")
    // } println(tr)
  }
}
