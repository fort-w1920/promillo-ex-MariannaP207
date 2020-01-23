#' Compute the amount of consumed alcohol
#'
#' Calculates the amount of consumed alcohol based on 4 alcohol drinks: "massn",
#' "hoibe", "wein", "schnaps"
#'
#' @param drinks list or vector with names "massn", "hoibe", "wein", "schnaps"
#' counting the number consumed of each type of drink
#' @return single numeric value of consumed alcohol in ml
#' @md
get_alcohol <- function(drinks) {

  volume <- c(
    "massn"    = 1000,
    "hoibe"   = 500,
    "wein"    = 200,
    "schnaps" = 40)
  alcohol_concentration <- c(
    "massn"    = 0.06,
    "hoibe"   = 0.06,
    "wein"    = 0.11,
    "schnaps" = 0.4)
  alcohol_density <- 0.8

  sum(drinks * volume[names(drinks)] *
        alcohol_concentration[names(drinks)] * alcohol_density)
}
