#' Compute permille value
#'
#' Calculates the BAC
#'
#' @param alcohol_drunk single numeric value of consumed alcohol
#' @param bodywater single numeric value of the body water index of a person
#' @param drinking_time sorted POSIXct vector giving start and end of the party
#' @return single numeric value of permille
#' @md
get_permille <- function(alcohol_drunk, bodywater, drinking_time){

  alcohol_density <- 0.8
  blood_density <- 1.055
  permille <- alcohol_density * alcohol_drunk / (blood_density * bodywater)

  partylength <- difftime(drinking_time[2], drinking_time[1], units = "hours")
  sober_per_hour <- 0.15
  # sobering up starts only after one hour & you can't be more sober than 0:
  max(0, permille  - (max(0, partylength - 1) * sober_per_hour))
}
