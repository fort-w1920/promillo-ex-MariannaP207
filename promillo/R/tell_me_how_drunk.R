#' Computation of Body Alcohol Concentration (BAC)
#'
#' Computes approximate BAC (in per mille) at the end of the party
#' as described [here](https://web.archive.org/web/20150123143123/http://promille-rechner.org/erlaeuterung-der-promille-berechnung/).
#'
#' @param age age of a person in years
#' @param sex single character "male" or "female"
#' @param height height of a person in cm
#' @param weight weight of a person in kg
#' @param drinking_time sorted POSIXct vector giving start and end of the party
#' @param drinks list or vector with names like "massn", "hoibe", "wein", "schnaps"
#' counting the consumed number of each type of beverage
#' @import checkmate
#' @return approximate BAC in per mille
#' @export
#' @md
tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {

  # check inputs
  drinks <- unlist(drinks)
  assert_subset(names(drinks),
    choices = c("massn", "hoibe", "wein", "schnaps"),
    empty.ok = FALSE
  )
  assert_numeric(drinks, lower = 0)

  assert_number(age, lower = 10, upper = 110)
  if (age < 16 | age > 90) {
    warning("...ts ts ts, this at your age!")
  }
  illegal <- (age < 16 & sum(drinks) > 0) |
    (age < 18 & isTRUE(drinks["schnaps"] > 0))
  if (illegal) {
    warning("\u2639 ...illegal  \u2639")
  }

  assert_number(height, lower = 100, upper = 230)
  assert_number(weight, lower = 40, upper = 300)
  assert_posixct(drinking_time, any.missing = FALSE, sorted = TRUE, len = 2)

  # calculate promille
  sex <- tolower(sex)
  sex <- match.arg(sex)

  alcohol_drunk <- get_alcohol(drinks)
  bodywater <- get_bodywater(sex, age, height, weight)
  get_permille(alcohol_drunk, bodywater, drinking_time)
}
