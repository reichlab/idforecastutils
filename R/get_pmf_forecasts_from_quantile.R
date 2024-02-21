#' Obtain pmf forecasts from quantile forecasts with custom categories
#'
#' @param quantile_forecasts a `model_out_tbl` of quantile forecasts with the
#'   following columns: `model_id`, `location`, `reference_date`, `horizon`,
#'   `target`, `target_end_date`, `output_type`, `output_type_id`, `value`
#' @param locations_df a data frame of locations to forecast for. Must contain
#'   the following columns: `geo_value` (lowercase abbreviations), `location`
#'   (fips codes), and `population`
#' @param truth_df a data frame of truth data with the following columns:
#'   `geo_value`, `time_value`, `value`
#' @param categories a vector of strings containing category names (without
#'   spaces).
#' @param horizons a vector of integers containing the horizons for which
#'   categories are defined. Defaults to 1, which should be used if categories
#'   are not horizon-specific.
#' @param count_rate_multiplier a matrix containing the count rate multipliers
#'   used to define categories. Must have dimension length(horizons) x
#'   length(categories)
#' @param category_rule a matrix containing the values against which to compare
#'   the count rates used to define categories when count rates are below the
#'   specified values. Must have dimension length(horizons) x length(categories)
#' @param target_name a string of the desired target name for the resulting
#'   pmf forecasts
#'
#' @return a `model_out_tbl` of both pmf and quantile forecasts constructed from
#'   the input quantile forecasts with the following columns: `model_id`,
#'   `location`, `reference_date`, `horizon`, `target`, `target_end_date`,
#'   `output_type`, `output_type_id`, `value`
#' @export
#'
#' @examples
#'
#' library(distfromq)
#' library(ggforce)
#' library(gridExtra)
#' library(reshape2)
#' horizons_2324_new <- 0:3
#' categories <-
#'   c("large_increase", "increase", "stable", "decrease", "large_decrease")
#' count_rate_multiplier_2324_new <-
#'   matrix(c(2:5, c(1, 1, 2, 2.5), c(-1, -1, -2, -2.5), -2:-5), ncol=4)
#' category_rule_2324_new <- matrix(c(rep(10, 8), rep(-10, 8)), ncol=4)

get_pmf_forecasts_from_quantile <- function(quantile_forecasts,
                                            locations_df, truth_df,
                                            categories,
                                            horizons = 1,
                                            count_rate_multiplier,
                                            category_rule,
                                            target_name =
                                              "wk flu hosp rate change") {

  # Coerce quantile_forecasts to model_out_tbl and validate
  if (!inherits(quantile_forecasts, "model_out_tbl")) {
    quantile_forecasts <- hubUtils::as_model_out_tbl(quantile_forecasts)
  }
  hubUtils::validate_model_out_tbl(quantile_forecasts)

  # Check quantile_forecasts has all required task ID columns
  req_quantile_tasks <-
    c("reference_date", "horizon", "target", "target_end_date", "location")
  if (!all(req_quantile_tasks %in% colnames(quantile_forecasts))) {
    cli::cli_abort(c(
      "x" = "{.arg quantile_forecasts} did not include required task ID columns
             {.val {req_quantile_tasks}}."
    ))
  }

  # Check locations_df has all required columns
  req_locations_cols <- c("geo_value", "location", "population")
  if (!all(req_locations_cols %in% colnames(locations_df))) {
    cli::cli_abort(c(
      "x" = "{.arg locations_df} did not include required columns
             {.val {req_locations_cols}}."
    ))
  }

  # Check truth_df has all required columns
  req_truth_cols <- c("geo_value", "time_value", "value")
  if (!all(req_truth_cols %in% colnames(truth_df))) {
    cli::cli_abort(c(
      "x" = "{.arg truth_df} did not include required columns
             {.val {req_truth_cols}}."
    ))
  }

  num_cat = length(categories)
  if (num_cat < 2) {
    cli::cli_abort(c(
      "x" = "{.arg categories} contains too few categories.",
      "i" = "At least 2 categories should be provided"
    ))
  }

  # Check count_rate_multiplier, category_rule matrices have correct dimensions
  crm_dim <- dim(count_rate_multiplier)
  cr_dim <- dim(category_rule)
  correct_dim <- c(length(horizons), length(categories) - 1)
  if (any(crm_dim != correct_dim)) {
    cli::cli_abort(c(
      "x" = "{.arg count_rate_multiplier} did not have the correct dimensions
             {.val {correct_dim}}."
    ))
  }
  if (any(cr_dim != correct_dim)) {
    cli::cli_abort(c(
      "x" = "{.arg category_rule} did not have the correct dimensions
             {.val {correct_dim}}."
    ))
  }


  model_output_cols <- 
    c("model_id", req_quantile_tasks, "output_type", "output_type_id", "value")
    
  truth_df_all <- truth_df |>
    dplyr::ungroup() |>
    dplyr::inner_join(locations_df, by = c("geo_value"))  |>
    dplyr::mutate(model_id = "Observed Data", 
                  reference_date = time_value + lubridate::weeks(1),
                  target_variable = target_name,
                  .before=1)

  # Calculate category boundary values
  criteria_df_temp <- truth_df_all
  criteria_df_all <- NULL
  for (j in 1:length(horizons)) {
    criteria_df_temp[["horizon"]] <- horizons[j]
    for (i in 1:(num_cat - 1)) {
      criteria_df_temp[[paste("crit", i, sep = "")]] <-
        ifelse(
          abs(criteria_df_temp[["population"]] * count_rate_multiplier[j, i] / 100000) <
            abs(category_rule[j, i]),
          criteria_df_temp[["value"]] + category_rule[j, i],
          criteria_df_temp[["value"]] +
            round(criteria_df_temp[["population"]] * count_rate_multiplier[j, i] / 100000, digits = 0)
        )
    }
    criteria_df_all <- dplyr::bind_rows(criteria_df_all, criteria_df_temp)
  }

  criteria_df_all <- criteria_df_all |>
    dplyr::select(model_id, location, value, reference_date, horizon, target_variable, population, crit1:ncol(criteria_df_all)) |>
    dplyr::filter(!is.na(value))

  train_forecasts <- criteria_df_all |>
    dplyr::select(location, reference_date, horizon, target_variable, population, crit1:ncol(criteria_df_all)) |>
    dplyr::mutate(target_end_date = reference_date + lubridate::weeks(horizon),
                  .before = target_variable)


  # extract log pdf and cdf values for training set forecasts
  quantile_forecasts <- quantile_forecasts |>
    dplyr::mutate(reference_date = as.Date(reference_date), output_type_id = as.numeric(output_type_id))
    
  # filter for dates, horizons, locations to forecast for
  criteria_df_filtered <- train_forecasts |>
    dplyr::inner_join(
      quantile_forecasts,
      by = c("reference_date", "horizon", "target_end_date", "location")
    )

  # filter for dates, horizons, locations to forecast for
  # (no distinct output_type_ids)
  train_forecasts <- criteria_df_filtered |>
    dplyr::distinct(model_id, location, reference_date, horizon, target,
                    .keep_all = TRUE) |>
    dplyr::select(-target, -output_type, -output_type_id, -value)

  # Calculate cdf category boundary values
  for (i in 1:(num_cat-1)) {
    criteria_df_filtered[["crit_current"]] <- criteria_df_filtered[[paste0("crit", i, sep = "")]]
    train_temp <- criteria_df_filtered |>
      dplyr::group_by(model_id, reference_date, horizon, target_end_date, 
                      target, location) |>
      dplyr::summarize(
        cdf_crit_current = distfromq::make_p_fn(
          ps = output_type_id,
          qs = value)(unique(crit_current), log = FALSE)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-c(target, target_end_date))
    train_forecasts[[paste0("cdf_crit", i)]] <- train_temp[["cdf_crit_current"]]
  }

  #calculate category percentages from cdf criteria, correcting for negative numbers
  exp_forecast <- train_forecasts |>
    dplyr::ungroup() |>
    dplyr::rename(target=target_variable) |>
    dplyr::mutate(cdf_crit0=1, .before=cdf_crit1)

  exp_forecast[[paste0("cdf_crit", num_cat)]] <-
    exp_forecast[[paste0("crit", num_cat)]] <- 0
  exp_forecast[["cdf_crit_sum"]] <- 0
  for (i in 1:(num_cat)) {
    exp_forecast[[categories[i]]] <-
      ifelse(round(exp_forecast[["cdf_crit_sum"]], digits=10) < 1,
             ifelse(exp_forecast[[paste0("crit", i)]] > 0,
                    exp_forecast[[paste0("cdf_crit", i-1)]] -
                      exp_forecast[[paste0("cdf_crit", i)]],
                    exp_forecast[[paste0("cdf_crit", i-1)]] - 0),
             0)
    exp_forecast[["cdf_crit_sum"]] <- 
      exp_forecast[["cdf_crit_sum"]] + exp_forecast[[categories[i]]]
  }

  exp_forecast <- exp_forecast |>
    dplyr::select(model_id, reference_date, location, horizon, target_end_date, all_of(categories))


  #transpose data_frame to format for submission
  exp_t <- reshape2::melt(
    exp_forecast,
    id.vars = c("model_id", "reference_date", "location", "horizon", "target_end_date"),
    measure.vars = categories,
    variable.name = "output_type_id",
    value.name = "value"
  )
  exp_t <- exp_t |>
    dplyr::mutate(target = target_name, output_type = "pmf") |>
    dplyr::select(dplyr::all_of(model_output_cols))

  output_forecasts <- exp_t |>
    dplyr::mutate(output_type_id = as.character(output_type_id)) |>
    dplyr::bind_rows(mutate(quantile_forecasts, 
                            output_type_id=as.character(output_type_id)))|>
    dplyr::filter(location %in% dplyr::pull(locations_df, location)) |>
    dplyr::select(dplyr::all_of(model_output_cols))

  return(output_forecasts)
}
