package zio.morphir.sexpr

import com.github.ghik.silencer.silent
import zio.Random
import zio.test.{Gen, Sized}

import java.math.BigInteger
import java.time._
import scala.collection.JavaConverters._
import scala.util.Try

object Gens {
  val genBigInteger: Gen[Random, BigInteger] =
    Gen
      .bigInt((BigInt(2).pow(128) - 1) * -1, BigInt(2).pow(128) - 1)
      .map(_.bigInteger)
      .filter(_.bitLength < 128)

  val genBigDecimal: Gen[Random, java.math.BigDecimal] =
    Gen
      .bigDecimal((BigDecimal(2).pow(128) - 1) * -1, BigDecimal(2).pow(128) - 1)
      .map(_.bigDecimal)
      .filter(_.toBigInteger.bitLength < 128)

  val genUsAsciiString: Gen[Random with Sized, String] =
    Gen.string(Gen.oneOf(Gen.char('!', '~')))

  val genAlphaLowerString: Gen[Random with Sized, String] =
    Gen.string(Gen.oneOf(Gen.char('a', 'z')))

  // Needs to be an ISO-8601 year between 0000 and 9999
  val anyIntYear: Gen[Random, Int] = Gen.int(0, 9999)

  val genYear: Gen[Random, Year] = anyIntYear.map(Year.of)

  val genLocalDate: Gen[Random, LocalDate] = for {
    year  <- genYear
    month <- Gen.int(1, 12)
    day   <- Gen.int(1, Month.of(month).length(year.isLeap))
  } yield LocalDate.of(year.getValue, month, day)

  val genLocalTime: Gen[Random, LocalTime] = for {
    hour   <- Gen.int(0, 23)
    minute <- Gen.int(0, 59)
    second <- Gen.int(0, 59)
    nano   <- Gen.int(0, 999999999)
  } yield LocalTime.of(hour, minute, second, nano)

  val genInstant: Gen[Random, Instant] = for {
    epochSecond     <- Gen.long(Instant.MIN.getEpochSecond, Instant.MAX.getEpochSecond)
    nanoAdjustment  <- Gen.long(Long.MinValue, Long.MaxValue)
    fallbackInstant <- Gen.elements(Instant.MIN, Instant.EPOCH, Instant.MAX)
  } yield Try(Instant.ofEpochSecond(epochSecond, nanoAdjustment)).getOrElse(fallbackInstant)

  val genZoneOffset: Gen[Random, ZoneOffset] = Gen.oneOf(
    Gen.int(-18, 18).map(ZoneOffset.ofHours),
    Gen.int(-18 * 60, 18 * 60).map(x => ZoneOffset.ofHoursMinutes(x / 60, x % 60)),
    Gen.int(-18 * 60 * 60, 18 * 60 * 60).map(ZoneOffset.ofTotalSeconds)
  )

  @silent("JavaConverters")
  val genZoneId: Gen[Random, ZoneId] = Gen.oneOf(
    genZoneOffset,
    genZoneOffset.map(zo => ZoneId.ofOffset("UT", zo)),
    genZoneOffset.map(zo => ZoneId.ofOffset("UTC", zo)),
    genZoneOffset.map(zo => ZoneId.ofOffset("GMT", zo)),
    Gen.elements(ZoneId.getAvailableZoneIds.asScala.toSeq: _*).map(ZoneId.of),
    Gen.elements(ZoneId.SHORT_IDS.values().asScala.toSeq: _*).map(ZoneId.of)
  )

  val genLocalDateTime: Gen[Random, LocalDateTime] = for {
    localDate <- genLocalDate
    localTime <- genLocalTime
  } yield LocalDateTime.of(localDate, localTime)

  val genZonedDateTime: Gen[Random, ZonedDateTime] = for {
    localDateTime <- genLocalDateTime
    zoneId        <- genZoneId
  } yield ZonedDateTime.of(localDateTime, zoneId)

  val genDuration: Gen[Random, Duration] = Gen.oneOf(
    Gen.long(Long.MinValue / 86400, Long.MaxValue / 86400).map(Duration.ofDays),
    Gen.long(Long.MinValue / 3600, Long.MaxValue / 3600).map(Duration.ofHours),
    Gen.long(Long.MinValue / 60, Long.MaxValue / 60).map(Duration.ofMinutes),
    Gen.long(Long.MinValue, Long.MaxValue).map(Duration.ofSeconds),
    Gen.long(Int.MinValue, Int.MaxValue.toLong).map(Duration.ofMillis),
    Gen.long(Int.MinValue, Int.MaxValue.toLong).map(Duration.ofNanos)
  )

  val genMonthDay: Gen[Random, MonthDay] = for {
    month <- Gen.int(1, 12)
    day   <- Gen.int(1, 29)
  } yield MonthDay.of(month, day)

  val genOffsetDateTime: Gen[Random, OffsetDateTime] = for {
    localDateTime <- genLocalDateTime
    zoneOffset    <- genZoneOffset
  } yield OffsetDateTime.of(localDateTime, zoneOffset)

  val genOffsetTime: Gen[Random, OffsetTime] = for {
    localTime  <- genLocalTime
    zoneOffset <- genZoneOffset
  } yield OffsetTime.of(localTime, zoneOffset)

  val genPeriod: Gen[Random, Period] = for {
    year  <- Gen.int
    month <- Gen.int
    day   <- Gen.int
  } yield Period.of(year, month, day)

  val genYearMonth: Gen[Random, YearMonth] = for {
    year  <- genYear
    month <- Gen.int(1, 12)
  } yield YearMonth.of(year.getValue, month)

  val genDayOfWeek: Gen[Random, DayOfWeek] = Gen.int(1, 7).map(DayOfWeek.of)

  val genMonth: Gen[Random, Month] = Gen.int(1, 12).map(Month.of)
}
