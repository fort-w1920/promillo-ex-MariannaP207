#' Plot BAC
#'
#' The function plots the reduction of the Body Alcohol Concentration index
#' in 5 minutes takt starting at the beginning of the party and its end.
#' For calculation of BAC, [promillo::tell_me_how_drunk] is used.
#'
#' @inheritParams tell_me_how_drunk
#' @import ggplot2
#' @importFrom lubridate minutes
#' @export
#' @md

show_me_how_drunk_2 <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {

  # check inputs
  drinks <- unlist(drinks)
  assert_subset(names(drinks),
                choices = c("massn", "hoibe", "wein", "schnaps"),
                empty.ok = FALSE
  )
  assert_numeric(drinks, lower = 0)

  assert_number(age, lower = 10, upper = 110)
  assert_number(height, lower = 100, upper = 230)
  assert_number(weight, lower = 40, upper = 300)
  assert_posixct(drinking_time, any.missing = FALSE, sorted = TRUE, len = 2)

  ## implementation
  diff <- as.numeric(difftime(drinking_time[2], drinking_time[1], units = "mins"))

  # calculate number of time points needed to achieve the drinking end
  # if the step size is 5 minutes
  n_steps <- floor(diff / 5)

  # create a time vector with 5 minutes step within drinking_time
  time_points <- as.POSIXct(NA)
  time_points[1] <- drinking_time[1]
  for (i in 1:n_steps) {
    time_points[i + 1] <- drinking_time[1] + lubridate::minutes(5 * i)
  }

  # create a vector with BAC values at each time point in time_points
  time_points <- as.character(time_points)
  promille_points <- vector(mode = "numeric", length = length(time_points))
  for (i in 1:length(promille_points)) {
    promille_points[i] <- tell_me_how_drunk(25, "male", 170, 70,
                                            drinking_time = as.POSIXct(c("2016-10-03 17:15:00", time_points[i])),
                                            drinks = c("massn" = 1)
    )
  }

  time_points <- as.POSIXct(time_points)
  message("Sobering up starts always after one hour of drinking.")

  ggplot(aes(time_points, promille_points),
         data = data.frame(time_points, promille_points)
  ) +
    geom_point() +
    labs(
      title = "Progress of sobering up",
      x = "Time point", y = "Promille in blood"
    ) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_datetime(date_breaks = "1 hours") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
