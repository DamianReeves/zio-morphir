package zio.morphir.ir.testing
import zio.test.*

object Gens {
  val greekLetterNames = Gen.weighted(
    Seq(
      "alpha",
      "beta",
      "gamma",
      "delta",
      "epsilon",
      "zeta",
      "eta",
      "theta",
      "iota",
      "kappa",
      "lambda",
      "mu",
      "nu",
      "xi",
      "omicron",
      "pi",
      "rho",
      "sigma",
      "tau",
      "upsilon",
      "phi",
      "chi",
      "psi",
      "omega"
    ).map(Gen.const(_) -> 12.0): _*
  )

  val monthNames = Gen.weighted(
    Seq(
      "January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December"
    ).map(Gen.const(_) -> 12.0): _*
  )

  val words = {
    val word = {
      val choices =
        Seq(
          "area",
          "benchmark",
          "book",
          "business",
          "company",
          "country",
          "currency",
          "day",
          "description",
          "entity",
          "fact",
          "family",
          "from",
          "government",
          "group",
          "home",
          "id",
          "job",
          "left",
          "lot",
          "market",
          "minute",
          "money",
          "month",
          "name",
          "number",
          "owner",
          "parent",
          "part",
          "problem",
          "rate",
          "right",
          "state",
          "source",
          "system",
          "time",
          "title",
          "to",
          "valid",
          "week",
          "work",
          "world",
          "year"
        ).map(Gen.const(_) -> 12.0)
      Gen.weighted(choices: _*)
    }

    word ++ greekLetterNames ++ monthNames
  }

}
