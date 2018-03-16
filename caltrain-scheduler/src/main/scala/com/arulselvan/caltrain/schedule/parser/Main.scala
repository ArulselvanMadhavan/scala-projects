package com.arulselvan.caltrain.schedule.parser

import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.browser.Browser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
// import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.Json
import scala.util.{Try, Success, Failure}
import java.io._

final case class TrainInfo(station: String,
                           zone: String,
                           northbound: List[String],
                           southbound: List[String])

final case class Stats(prev: Option[TimeFormat],
                       timeFormat: String,
                       result: List[String])

final case class Stations(stations:Iterable[String])

final case class TimeFormat(hour: Int, minutes: Int)
    extends Ordered[TimeFormat] {

  def compare(that: TimeFormat): Int = {
    (this.hour compare that.hour) match {
      case 0 => this.minutes compare that.minutes
      case x => x
    }
  }

  override def toString(): String = {
    val hours = "%02d".format(this.hour)
    val minutes = "%02d".format(this.minutes)
    s"${hours}:${minutes}"
  }
}

object Main {
  val CALTRAIN_SITE = "http://www.caltrain.com/schedules/weekdaytimetable.html"

  def makeInt(x: String): Try[Int] = Try(x.trim.filter(_.isDigit).toInt)

  private[this] def convertToTimeFormat(
      str: String): Either[String, TimeFormat] = {
    val hoursAndMinutes: Array[String] = str.split(":")
    if (hoursAndMinutes.length != 2)
      Left(s"Incorrect timeformat exception${str}")
    else {
      val hours = makeInt(hoursAndMinutes(0))
      val minutes = makeInt(hoursAndMinutes(1))
      (hours, minutes) match {
        case (Success(h), Success(m))
            if h >= 0 && h <= 12 && m >= 0 && m <= 59 =>
          Right(TimeFormat(h, m))
        case (Failure(h), _) => Left(h.getMessage)
        case (_, Failure(m)) => Left(m.getMessage)
        case _               => Left(s"Invalid Hours and minutes${hoursAndMinutes}")
      }
    }
  }

  private[this] def attachAmPm(times: Iterable[String]): List[String] = {
    val timeFormats: Iterable[Either[String, TimeFormat]] =
      times.map(convertToTimeFormat)
    val successFormats = timeFormats.map(_.toOption).flatten
    val initial = Stats(None: Option[TimeFormat], "am", Nil: List[String])
    val results = successFormats.foldLeft(initial) {
      case (Stats(prev, tf, acc), cur) =>
        val meridiem =
          prev.fold("am")(prevVal => if (cur < prevVal) "pm" else tf)
        val nextVal = s"${cur} ${meridiem}"
        Stats(Some(cur), meridiem, nextVal :: acc)
    }
    results.result
  }

  private[this] def constructJsonFormat(nbTrain: Element,
                                        sbTrain: Element): Json = {
    val stationName: String = nbTrain >> text("th a")
    val zoneId: String = nbTrain >> text("th")
    val toNorth: Iterable[String] =
      (nbTrain >> texts("td"))
        .filterNot(_.isEmpty)
        .filterNot(_ == "–")
        .filterNot(_ == " ")
    val toSouth: Iterable[String] =
      (sbTrain >> texts("td"))
        .filterNot(_.isEmpty)
        .filterNot(_ == "–")
        .filterNot(_ == " ")
    val northBoundTrains = attachAmPm(toNorth)
    val southBoundTrains = attachAmPm(toSouth)
    TrainInfo(stationName, zoneId, northBoundTrains, southBoundTrains).asJson
  }

  private[this] def writeJsonToFile(fileName: String, text:Json): Unit = {
    val file = new File(fileName)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(text.toString)
    bw.close()
  }

  def main(args: Array[String]): Unit = {
    val browser: Browser = JsoupBrowser()
    val doc = browser.get(CALTRAIN_SITE)
    val northboundTT: Element = doc >> element(".NB_TT")
    val southboundTT: Element = doc >> element(".SB_TT")
    val nbTrains = northboundTT >> elementList("tbody tr")
    val sbTrains = (southboundTT >> elementList("tbody tr")).reverse
    val allTrains = (nbTrains zip sbTrains)
    val allStations:Iterable[Json] = for {
      (nbTrain, sbTrain) <- allTrains
    } yield constructJsonFormat(nbTrain, sbTrain)
    writeJsonToFile("allStations.json", allStations.asJson);
  }
}
