#' Get the bin endpoints for FluSight categorical targets
#'
#' @param target_ts Data frame with target time series data for FluSight,
#' including the columns `location`, `date`, and `value`
#' @param location_meta Data frame with metadata about locations for FluSight,
#' matching the format of the file in cdcepi/FluSight-forecast-hub/auxiliary-data/locations.csv
#' @param season String naming the season: only "2023/24" and "2024/25" are supported
#'
#' @details Compute the bin endpoints used for categorical targets in FluSight
#' in the 2023/24 or 2024/25 season. In that season, there were 5 categories:
#' "large decrease", "decrease", "stable", "increase", and "large increase".
#' The bin endpoints have the form
#' value +/- max(multiplier * population / 100k, min_count),
#' where `value` denotes the most recently observed data value, and the
#' multiplier and minimum count threshold were potentially specific to the
#' bin.
#'
#' @return a data frame with columns "location", "output_type_id", "lower", "upper",
#' suitable for use as an input to transform_quantile_to_pmf.
#'
#' @export
get_flusight_bin_endpoints <- function(target_ts, location_meta, season) {
  location_meta <- location_meta |>
    dplyr::mutate(pop100k = .data[["population"]] / 100000) |>
    dplyr::select(dplyr::all_of(c("location", "pop100k")))

  bin_endpoint_meta <- get_flusight_bin_endpoint_meta(season)

  if (season == "2022/23") {
    result_cols <- c("location", "output_type_id", "lower", "upper")
  } else {
    result_cols <- c("location", "horizon", "output_type_id", "lower", "upper")
  }

  bin_endpoints <- target_ts |>
    dplyr::select(dplyr::all_of(c("location", "date", "value"))) |>
    dplyr::filter(date == max(.data[["date"]])) |>
    dplyr::left_join(
      location_meta,
      by = "location"
    ) |>
    dplyr::cross_join(
      bin_endpoint_meta
    ) |>
    dplyr::mutate(
      lower = .data[["value"]] + .data[["lower_sign"]] *
        pmax(.data[["lower_rate_multiplier"]] * .data[["pop100k"]],
             .data[["lower_min_count_change"]]),
      upper = .data[["value"]] + .data[["upper_sign"]] *
        pmax(.data[["upper_rate_multiplier"]] * .data[["pop100k"]],
             .data[["upper_min_count_change"]])
    ) |>
    dplyr::select(dplyr::all_of(result_cols))

  return(bin_endpoints)
}


get_flusight_bin_endpoint_meta <- function(season = "2023/24") {
  # we use 9.5 for the minimum count change because for low-population states,
  # the intervals have the half-open form (value - 9.5, value + 9.5] for stable,
  # (value - ***, value - 9.5] for decrease and large decrease,
  # (value + 9.5, value + ***] for increase and large increase,
  # where *** is something coming from a population rate per 100k since all
  # state populations / 100000 are greater than 5
  # In all cases, this says a count change of less than 10 is stable
  # and a count change of 10 or more is non-stable
  bin_endpoint_meta <- tidyr::expand_grid(
    horizon = 0:3,
    output_type_id = list(c("large_decrease", "decrease", "stable", "increase", "large_increase")),
    lower_min_count_change = 9.5,
    upper_min_count_change = 9.5
  )

  if (!(season %in% c("2023/24", "2024/25"))) {
    stop("unsupported season")
  } else if (season == "2023/24") {
    bin_endpoint_meta$rate_multiplier_endpoints <- list(
      c(-Inf, -2, -1, 1, 2, Inf),
      c(-Inf, -3, -1, 1, 3, Inf),
      c(-Inf, -4, -2, 2, 4, Inf),
      c(-Inf, -5, -2.5, 2.5, 5, Inf)
    )
  } else if (season == "2024/25") {
    bin_endpoint_meta$rate_multiplier_endpoints <- list(
      c(-Inf, -1.7, -0.3, 0.3, 1.7, Inf),
      c(-Inf, -3, -0.5, 0.5, 3, Inf),
      c(-Inf, -4, -0.7, 0.7, 4, Inf),
      c(-Inf, -5, -1, 1, 5, Inf)
    )
  }

  bin_endpoint_meta <- bin_endpoint_meta |>
    dplyr::mutate(
      lower_rate_multiplier = lapply(
        .data[["rate_multiplier_endpoints"]],
        function(x) {
          x[1:(length(x) - 1)]
        }
      ),
      upper_rate_multiplier = lapply(
        .data[["rate_multiplier_endpoints"]],
        function(x) {
          x[2:length(x)]
        }
      )
    ) |>
    dplyr::select(-dplyr::all_of("rate_multiplier_endpoints")) |>
    tidyr::unnest(cols = c("output_type_id", "lower_rate_multiplier", "upper_rate_multiplier")) |>
    dplyr::mutate(
      lower_sign = sign(.data[["lower_rate_multiplier"]]),
      lower_rate_multiplier = abs(.data[["lower_rate_multiplier"]]),
      upper_sign = sign(.data[["upper_rate_multiplier"]]),
      upper_rate_multiplier = abs(.data[["upper_rate_multiplier"]])
    ) |>
    dplyr::select(dplyr::all_of(
      c("horizon", "output_type_id",
        "lower_sign", "lower_rate_multiplier", "lower_min_count_change",
        "upper_sign", "upper_rate_multiplier", "upper_min_count_change")
    ))

  return(bin_endpoint_meta)
}
