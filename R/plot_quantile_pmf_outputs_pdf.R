#' Plot the quantile and pmf output type for a model_out_tbl to be saved as a PDF
#'
#' @param model_out_tbl an object of class `model_out_tbl` with component model
#'   outputs (e.g., predictions). Assumes "location" and "horizon" are among the
#'   task ID cols. Should only contain one model.
#' @param target_ts Data frame with target time series data for FluSight,
#'   including the columns `location`, `date`, and `value`
#' @param location_meta a `data.frame` containing information about the locations
#'   being forecast. Assumed to contain a "location" column that may be joined with
#'   that in the provided `model_out_tbl` and `target_ts`, plus a "location_name"
#'   column of full location names (instead of abbreviations or fips codes). Any
#'   other columns will be ignored. Defaults to NULL, in which case the values in
#'   the "location" column will be used as plot labels.
#' @param reference_date string of the reference date for the forecasts.
#'   Must be in the ymd format, with yyyy-mm-dd format recommended.
#' @param intervals numeric vector of prediction interval levels to plot for the
#'   quantile output type. Provided levels may be 0.5, 0.8, 0.9, 0.95.
#'   NULL means no interval levels are plotted. Defaults to c(.5, .8, .95).
#' @param cats_ordered character vector ordering the pmf output type IDs, which
#'   provides the order in which the categories are stacked in the output plot.
#'   Defaults to NULL, in which case pmf categories are ordered alphabetically.
#' @param quantile_title character string providing a title for the plot of
#'   quantile forecasts. Defaults to NULL, in which case no title is used.
#' @param pmf_title character string providing a title for the plot of pmf
#'   forecasts. Defaults to NULL, in which case no title is used.
#'
#' @return NULL
#'
#' @export
plot_quantile_pmf_outputs_pdf <- function(model_out_tbl, target_ts,
                                          location_meta = NULL, reference_date,
                                          intervals = c(0.5, 0.8, 0.95), cats_ordered = NULL,
                                          quantile_title = NULL, pmf_title = NULL) {

  if (!inherits(model_out_tbl, "model_out_tbl")) {
    model_out_tbl <- hubUtils::as_model_out_tbl(model_out_tbl)
  }

  forecast_types <- unique(model_out_tbl$output_type)
  if (!all(c("quantile", "pmf") %in% forecast_types)) {
    cli::cli_abort("{.arg model_out_tbl} must contain both the quantile and pmf output type")
  }

  if (is.null(location_meta)) {
    location_col <- "location"
  } else {
    req_loc_col <- c("location", "location_name")
    if (!all(req_loc_col %in% names(location_meta))) {
      cli::cli_abort("{.arg location_meta} must contain the columns {.val {req_loc_col}}")
    }
    location_col <- "location_name"
    model_out_tbl <- dplyr::left_join(model_out_tbl, location_meta, by = "location")
    target_ts <- dplyr::left_join(target_ts, location_meta, by = "location")
  }

  if (length(reference_date) > 1) {
    cli::cli_abort("Only one {.arg reference_date} may be provided")
  } else {
    reference_date <- validate_ymd_date(reference_date, arg_name = "reference_date")
  }

  cats_actual <- unique(model_out_tbl$output_type_id[model_out_tbl$output_type == "pmf"])
  if ((!is.character(cats_ordered) && !is.null(cats_ordered)) || !setequal(cats_ordered, cats_actual)) {
    cli::cli_abort("{.arg cats_ordered} must be a character vector ordering the
                    provided pmf output type IDs in {.arg model_out_tbl}")
  }

  model_id <- unique(model_out_tbl$model_id)
  if (length(model_id) > 1) {
    cli::cli_abort("Plotting both quantile and pmf forecasts in this format for 
                    multiple models is not recommended.")
  }

  # plot forecasts for each location on a single page
  data_start <- reference_date - 12 * 7
  data_end <- reference_date + 6 * 7

  purrr::map(unique(model_out_tbl$location), .f = function(loc) {
    p1 <- model_out_tbl |>
      dplyr::filter(.data[["output_type"]] == "quantile", .data[["location"]] == loc) |>
      dplyr::mutate(output_type_id = as.numeric(.data[["output_type_id"]])) |>
      hubVis::plot_step_ahead_model_output(
        target_ts |>
          dplyr::mutate(observation = .data[["value"]]) |>
          dplyr::filter(date >= data_start, date <= data_end, .data[["location"]] == loc),
        x_col_name = "target_end_date",
        x_target_col_name = "date",
        intervals = intervals,
        facet = location_col,
        facet_scales = "free_y",
        facet_nrow = 1,
        use_median_as_point = TRUE,
        interactive = FALSE,
        show_plot = FALSE,
        title = NULL,
        group = "reference_date"
      ) +
      ggplot2::theme(
        legend.position = "inside",
        legend.position.inside = c(.05, .95),
        legend.justification = c(0, 1),
        legend.key = ggplot2::element_rect(colour = "transparent", fill = "white"),
        legend.background = ggplot2::element_rect(scales::alpha("white", 0.5)),
        legend.box = "horizontal"
      ) +
      ggplot2::labs(title = quantile_title, subtitle = paste("Selected reference date:", reference_date))

    if (is.null(cats_ordered)) cats_ordered <- cats_actual

    p2 <- model_out_tbl |>
      dplyr::filter(.data[["output_type"]] == "pmf", .data[["location"]] == loc) |>
      dplyr::mutate(output_type_id = forcats::fct_relevel(.data[["output_type_id"]], cats_ordered)) |>
      ggplot2::ggplot(ggplot2::aes(fill = .data[["output_type_id"]], y = .data[["value"]], x = .data[["horizon"]])) +
      ggplot2::geom_bar(position = "stack", stat = "identity") +
      ggplot2::labs(title = pmf_title)

    gridExtra::grid.arrange(p1, p2, nrow = 1, widths = c(5, 3))
  })

  grDevices::dev.off()
}


#' Validate value to be converted into a ymd date
#'
#' @param date value to be converted into a ymd date
#' @param arg_name character string name of the argument being validated to be
#'   printed in the error message (generally the name of the `date` object)
#'
#' @return a validated Date object (or vector) in the ymd format
#'
#' @noRd
validate_ymd_date <- function(date, arg_name) {
  if (is.null(date)) {
    cli::cli_abort("{.arg {arg_name}} is missing")
  }

  ymd_date <- lubridate::ymd(date, quiet = TRUE)
  if (is.na(ymd_date)) {
    cli::cli_abort("{.arg {arg_name}} could not be correctly parsed. Please use the ymd format")
  } else {
    ymd_date
  }
}
