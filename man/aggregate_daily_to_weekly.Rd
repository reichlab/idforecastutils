% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_and_format_daily_data.R
\name{aggregate_daily_to_weekly}
\alias{aggregate_daily_to_weekly}
\title{Aggregate daily data to a weekly time scale, with weeks ending on Saturday}
\usage{
aggregate_daily_to_weekly(daily_data)
}
\arguments{
\item{daily_data}{a data frame of daily data with the following columns:
\code{geo_value}, \code{location}, \code{time_value}, \code{value}}
}
\value{
a data frame of weekly data with the same columns as the input data:
\code{geo_value}, \code{location}, \code{time_value}, \code{value}
}
\description{
Aggregate daily data to a weekly time scale, with weeks ending on Saturday
}
