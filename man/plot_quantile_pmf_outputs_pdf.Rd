% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_quantile_pmf_outputs_pdf.R
\name{plot_quantile_pmf_outputs_pdf}
\alias{plot_quantile_pmf_outputs_pdf}
\title{Plot the quantile and pmf output type for a model_out_tbl to be saved as a PDF}
\usage{
plot_quantile_pmf_outputs_pdf(
  model_out_tbl,
  target_ts,
  location_meta = NULL,
  reference_date,
  intervals = c(0.5, 0.8, 0.95),
  cats_ordered = NULL,
  quantile_title = NULL,
  pmf_title = NULL
)
}
\arguments{
\item{model_out_tbl}{an object of class \code{model_out_tbl} with component model
outputs (e.g., predictions). Assumes "location" and "horizon" are among the
task ID cols. Should only contain one model.}

\item{target_ts}{Data frame with target time series data for FluSight,
including the columns \code{location}, \code{date}, and \code{value}}

\item{location_meta}{a \code{data.frame} containing information about the locations
being forecast. Assumed to contain a "location" column that may be joined with
that in the provided \code{model_out_tbl} and \code{target_ts}, plus a "location_name"
column of full location names (instead of abbreviations or fips codes). Any
other columns will be ignored. Defaults to NULL, in which case the values in
the "location" column will be used as plot labels.}

\item{reference_date}{string of the reference date for the forecasts.
Must be in the ymd format, with yyyy-mm-dd format recommended.}

\item{intervals}{numeric vector of prediction interval levels to plot for the
quantile output type. Provided levels may be 0.5, 0.8, 0.9, 0.95.
NULL means no interval levels are plotted. Defaults to c(.5, .8, .95).}

\item{cats_ordered}{character vector ordering the pmf output type IDs, which
provides the order in which the categories are stacked in the output plot.
Defaults to NULL, in which case pmf categories are ordered alphabetically.}

\item{quantile_title}{character string providing a title for the plot of
quantile forecasts. Defaults to NULL, in which case no title is used.}

\item{pmf_title}{character string providing a title for the plot of pmf
forecasts. Defaults to NULL, in which case no title is used.}
}
\description{
Plot the quantile and pmf output type for a model_out_tbl to be saved as a PDF
}
