% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/show_me_how_drunk_2.R
\name{show_me_how_drunk_2}
\alias{show_me_how_drunk_2}
\title{Plot BAC}
\usage{
show_me_how_drunk_2(age, sex = c("male", "female"), height, weight,
  drinking_time, drinks)
}
\arguments{
\item{age}{age of a person in years}

\item{sex}{single character "male" or "female"}

\item{height}{height of a person in cm}

\item{weight}{weight of a person in kg}

\item{drinking_time}{sorted POSIXct vector giving start and end of the party}

\item{drinks}{list or vector with names like "massn", "hoibe", "wein", "schnaps"
counting the consumed number of each type of beverage}
}
\description{
The function plots the reduction of the Body Alcohol Concentration index
in 5 minutes takt starting at the beginning of the party and its end.
For calculation of BAC, \link[promillo:tell_me_how_drunk]{promillo::tell_me_how_drunk} is used.
}
