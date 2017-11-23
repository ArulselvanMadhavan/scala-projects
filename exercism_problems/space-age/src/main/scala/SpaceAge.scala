import scala.language.postfixOps

object SpaceAge {
  final val EARTH_DAYS_IN_YEAR = 365.25

  def earthYearInSecs(secs: Double, factor: Double): Double =
    (secs / (EARTH_DAYS_IN_YEAR * 24 * 60 * 60)) / factor

  def format(d: Double): Double =
    BigDecimal(d) setScale (2, BigDecimal.RoundingMode.HALF_EVEN) toDouble

  def onEarth(secs: Double): Double =
    format(earthYearInSecs(secs, 1.0))

  def onMercury(secs: Double): Double =
    format(earthYearInSecs(secs, 0.2408467))

  def onVenus(secs: Double): Double =
    format(earthYearInSecs(secs, 0.61519726))

  def onMars(secs: Double): Double =
    format(earthYearInSecs(secs, 1.8808158))

  def onJupiter(secs: Double): Double =
    format(earthYearInSecs(secs, 11.862615))

  def onSaturn(secs: Double): Double =
    format(earthYearInSecs(secs, 29.447498))

  def onUranus(secs: Double): Double =
    format(earthYearInSecs(secs, 84.016846))

  def onNeptune(secs: Double): Double =
    format(earthYearInSecs(secs, 164.79132))
}
