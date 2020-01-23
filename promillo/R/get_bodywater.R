#' Compute body water index
#'
#' Calculates the body water index based on the physical properties of a person
#'
#' @param sex single character as "male" or "female"
#' @param age age of a person in years
#' @param height height of a person in cm
#' @param weight weight of a person in kg
#' @return single numeric value of bodywater index
#' @md
get_bodywater <- function(sex = c("male", "female"), age, height, weight) {

  coef <- if (sex == "male") {
    c(2.447, -0.09516, 0.1074, 0.3362)
  } else {
    c(0.203, -0.07, 0.1069, 0.2466)
  }
  t(coef) %*% c(1, age, height, weight)
}

