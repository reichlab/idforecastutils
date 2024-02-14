#' Load and format infectious disease target data queried from covidData
#'
#' @param disease a string of the diseased query data for. Must be one of "flu"
#'   (flu hospitalizations) or "covid" (covid hospitalizations).
#' @param locations_df a data frame of locations to forecast for. Must contain 
#'   the following columns: `geo_value` (lowercase abbreviations), and `location` 
#'   (fips codes). Defaults to NULL.
#' @param due_date a string specifying the date as of for which to query the data
#'
#' @return a data frame of daily data for the specified disease with the 
#' following columns: `geo_value`, `location`, `time_value`, `value`
#' @export
#'
#' @examples

load_format_daily_data <- function(disease, locations_df=NULL, due_date=NULL) {

  daily_data <- covidData::load_data(
    as_of = due_date,
    spatial_resolution = c("national", "state"),
    temporal_resolution = "daily",
    measure = ifelse(disease == "flu", "flu hospitalizations", "hospitalizations"),
    drop_last_date = FALSE # changed from TRUE
  ) |>
    dplyr::left_join(covidData::fips_codes, by = "location") |>
    dplyr::transmute(
      date=date+days(1), # for proper alignment
      location,
      location_name = ifelse(location_name == "United States", "US", location_name),
      value = inc
    ) |>
    dplyr::arrange(location, date)
  
  if (!is.null(locations_df)) {
    daily_data <- daily_data |>
      dplyr::filter(location %in% pull(locations_df, location))
      dplyr::left_join(locations_df, by = "location") |>
      dplyr::mutate(geo_value = tolower(abbreviation)) |>
      dplyr::select(geo_value, location, time_value = date, value)
  }
    
  return(daily_data)
}

#' Aggregate daily data to a weekly time scale, with weeks ending on Saturday
#'
#' @param daily_data a data frame of daily data with the following columns: 
#'   `geo_value`, `location`, `time_value`, `value`
#'
#' @return a data frame of weekly data with the same columns as the input data:
#' `geo_value`, `location`, `time_value`, `value`
#' @export
#'
#' @examples

aggregate_daily_to_weekly <- function(daily_data) {
  last_data_saturday <- daily_data |>
    dplyr::distinct(time_value, .keep_all = TRUE) |>
    dplyr::slice_max(time_value, n = 7) |>
    dplyr::mutate(day = lubridate::wday(time_value, label=TRUE, abbr=TRUE)) |>
    dplyr::filter(day=="Sat") |>
    dplyr::pull(time_value)

  weekly_data <- daily_data |>
    dplyr::filter(time_value <= last_truth_saturday) |>
    dplyr::mutate(associated_saturday = lubridate::ceiling_date(time_value, "week") - days(1)) |>
    dplyr::group_by(location, associated_saturday) |>
    dplyr::summarize(week_value=sum(value)) |>
    dplyr::select(-time_value, -value) |>
    dplyr::rename(time_value=associated_saturday, value=week_value) 

  return (weekly_data)
}
