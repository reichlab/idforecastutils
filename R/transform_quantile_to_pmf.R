#' Transform model outputs from the quantile `output_type` to the pmf
#' `output_type`.
#'
#' @param model_out_tbl An object of class `model_out_tbl` containing
#' predictions of quantile `output_type`
#' @param bin_endpoints A data frame with columns:
#'   - `output_type_id`: character or numeric vector of labels for each bin or
#'     category
#'   - `lower`: numeric vector of lower endpoints for each bin
#'   - `upper`: numeric vector of upper endpoints for each bin
#' Optionally, this data frame may also contain other columns matching task id
#' variables in `model_out_tbl`, if the bin endpoints differ depending on the
#' prediction task (e.g. for different locations).
#' @param tail_dist: String specifying the distribution to use for
#' extrapolation into the tails of the predictive distribution. See the details
#' for more information.
#'
#' @details Bin probabilities are calculated by taking a difference of values of
#' the estimated CDF at the lower and upper endpoints of each bin. This means
#' that probabilities are for half-open intervals of the form (lower, upper].
#'
#' The CDF is estimated using methods from the distfromq package. In brief,
#' we use a monotonic cubic spline to interpolate the quantiles, and use
#' members of a specified parametric family (by default, normal distributions)
#' to extrapolate into the tails. See [distfromq::make_p_fn] for details.
#'
#' @return model_out_tbl with pmf forecasts
#'
#' @export
#'
#' @importFrom rlang .data
transform_quantile_to_pmf <- function(model_out_tbl,
                                      bin_endpoints,
                                      tail_dist = "norm") {
  model_out_tbl <- val_transform_q_to_p_args(model_out_tbl, bin_endpoints)
  do_transform_q_to_p(model_out_tbl, bin_endpoints, tail_dist)
}

#' Perform the operation of transforming quantile forecasts to pmf forecasts
#' @noRd
do_transform_q_to_p <- function(model_out_tbl, bin_endpoints, tail_dist) {
  task_id_cols <- get_task_id_cols(model_out_tbl)
  join_cols <- task_id_cols[task_id_cols %in% colnames(bin_endpoints)]

  # get one estimated cdf per model and prediction task
  model_out_tbl <- model_out_tbl |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("model_id", task_id_cols)))) |>
    dplyr::summarize(
      p_fn = list(distfromq::make_p_fn(
        ps = as.numeric(.data[["output_type_id"]]),
        qs = as.numeric(.data[["value"]]),
        tail_dist = tail_dist
      ))
    )

  # join with bin endpoints
  # many-to-many relationship because:
  # - bin_endpoints may have a subset of task id columns, does not have model_id
  # - bin_endpoints has many output_type_ids
  # dplyr complains if we try to join on an empty character vector,
  # so we cross_join in that case
  if (length(join_cols) > 0) {
    model_out_tbl <- model_out_tbl |>
      dplyr::full_join(
        bin_endpoints,
        by = join_cols,
        relationship = "many-to-many"
      )
  } else {
    model_out_tbl <- model_out_tbl |>
      dplyr::cross_join(
        bin_endpoints
      )
  }

  # compute class probabilities, clean up included columns
  model_out_tbl <- model_out_tbl |>
    dplyr::mutate(
      value = purrr::pmap_dbl(
        list(.data[["p_fn"]], .data[["lower"]], .data[["upper"]]),
        function(p_fn, lower, upper) {
          p_fn(upper) - p_fn(lower)
        }
      )
    ) |>
    dplyr::mutate(
      output_type = "pmf",
      .before = "output_type_id"
    ) |>
    dplyr::select(
      -dplyr::all_of(c("lower", "upper", "p_fn"))
    )

  return(model_out_tbl)
}

#' Get task id columns
#' @noRd
get_task_id_cols <- function(model_out_tbl) {
  model_out_cols <- colnames(model_out_tbl)
  non_task_cols <- c("model_id", "output_type", "output_type_id", "value")
  task_id_cols <- model_out_cols[!model_out_cols %in% non_task_cols]

  return(task_id_cols)
}

#' Do validation of arguments to transform_quantile_to_pmf
#' @noRd
val_transform_q_to_p_args <- function(model_out_tbl, bin_endpoints) {
  if (!inherits(model_out_tbl, "model_out_tbl")) {
    model_out_tbl <- hubUtils::as_model_out_tbl(model_out_tbl)
  }
  task_id_cols <- get_task_id_cols(model_out_tbl)

  if (!is.data.frame(bin_endpoints)) {
    cli::cli_abort("{.arg bin_endpoints} must be a data frame.")
  }

  val_bin_endpoint_colnames(model_out_tbl, bin_endpoints, task_id_cols)
  val_bin_endpoint_task_groups(model_out_tbl, bin_endpoints, task_id_cols)
  val_bin_endpoint_lower_upper(bin_endpoints, task_id_cols)

  return(model_out_tbl)
}

#' Validate the column names of bin_endpoint
#' @noRd
val_bin_endpoint_colnames <- function(model_out_tbl, bin_endpoints, task_id_cols) {
  existing_cols <- colnames(bin_endpoints)
  required_cols <- c("output_type_id", "lower", "upper")
  missing_cols <- required_cols[!required_cols %in% existing_cols]
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "{.arg bin_endpoints} was missing {length(missing_cols)} required column{?s}.",
      "i" = "Required columns are {required_cols}.",
      "!" = "Missing columns: {missing_cols}."
    ))
  }

  expected_cols <- c(required_cols, task_id_cols)
  unexpected_cols <- existing_cols[!existing_cols %in% expected_cols]
  if (length(unexpected_cols) > 0) {
    cli::cli_abort(c(
      "{.arg bin_endpoints} had {length(unexpected_cols)} unexpected column name{?s}.",
      "i" = "Expected column names to be a subset of {expected_cols}.",
      "!" = "Unexpected columns: {unexpected_cols}."
    ))
  }
}

#' Check that within each group defined by task id variables,
#' all output_type_id values appear in bin_endpoints and
#' no duplicate output_type_id values
#' @noRd
val_bin_endpoint_task_groups <- function(model_out_tbl, bin_endpoints, task_id_cols) {
  group_vars <- task_id_cols[task_id_cols %in% colnames(bin_endpoints)]
  if (length(group_vars) > 0) {
    output_type_id_sets <- bin_endpoints |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarize(
        otid = list(output_type_id),
        .groups = "drop"
      ) |>
      dplyr::distinct(otid)
    if (nrow(output_type_id_sets) > 1) {
      cli::cli_abort(
        "{.arg bin_endpoints} must contain the same `output_type_id` values for each group defined by task id variables."
      )
    }
  }

  any_dups_by_group <- bin_endpoints |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarize(
      any_dups = any(duplicated(.data[["output_type_id"]])),
      .groups = "drop"
    ) |>
    dplyr::filter(.data[["any_dups"]])
  if (nrow(any_dups_by_group) > 0) {
    failed_group <- any_dups_by_group |>
      dplyr::select(-dplyr::all_of("any_dups")) |>
      head(1)
    cli::cli_abort(c(
      "{.arg bin_endpoints} must not contain duplicated `output_type_id` values within any group defined by task id variables."
    ))
  }

}

#' Check that bin lower/upper endpoints are nonmissing and that
#' within each group defined by task id variables,
#' all bin upper endpoints match the lower endpoint of the next bin
#' @noRd
val_bin_endpoint_lower_upper <- function(bin_endpoints, task_id_cols) {
  if (any(is.na(bin_endpoints$lower)) || any(is.na(bin_endpoints$upper))) {
    cli::cli_abort(
      "{.arg bin_endpoints} must not have any missing values in its `lower` and `upper` columns."
    )
  }

  group_vars <- task_id_cols[task_id_cols %in% colnames(bin_endpoints)]
  endpoints_match <- bin_endpoints |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::mutate(
      next_lower = dplyr::lead(.data[["lower"]], n = 1),
      endpoints_match = (.data[["upper"]] == .data[["next_lower"]])
    ) |>
    dplyr::summarize(
      endpoints_match = all(endpoints_match, na.rm = TRUE),
      .groups = "drop"
    )
  if (any(!endpoints_match$endpoints_match)) {
    failed_group <- dplyr::filter(endpoints_match, !.data[["endpoints_match"]]) |>
      head(1) |>
      dplyr::select(-dplyr::all_of("endpoints_match"))

    cli::cli_abort(c(
      "In {.arg bin_endpoints}, the upper endpoint of each bin must equal the lower endpoint of the next bin.",
      "i" = "First group that did not meet this condition: {failed_group}"
    ))
  }
}
