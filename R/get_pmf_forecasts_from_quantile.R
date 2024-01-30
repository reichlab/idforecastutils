#' Obtain pmf forecasts from quantile forecasts for flu hosp rate change categories
#'
#' @param quantile_forecasts a `model_out_tbl` of quantile forecasts with the 
#'   following columns: `model_id`, `location`, `reference_date`, `horizon`, 
#'   `target`, `target_end_date`, `output_type`, `output_type_id`, `value`
#' @param locations_df a data frame of locations to forecast for. Must contain 
#'   the following columns: `geo_value` (lowercase abbreviations), `location` 
#'   (fips codes), location_name (full name), and `population`
#' @param truth_df a data frame of truth data with the following columns: 
#'   `geo_value`, `time_value`, `value`
#' @param categories a vector of strings containing category names (without spaces).
#' @param horizons a vector of integers containing the horizons for which categories
#'   are defined. Defaults to 1, which should be used if categories are not 
#'   horizon-specific.
#' @param count_rate_multiplier a matrix containing the count rate multipliers used
#'   to define categories. Must have dimension length(horizons) x length(categories)
#' @param category_rule a matrix containing the values against which to compare the
#'   count rates used to define categories when count rates are below the specified
#'   values. Must have dimension length(horizons) x length(categories)
#' @param target_name a string of the desired target name for the output forecasts
#' @param save_path a string of the path to which to save the output forecasts
#'
#' @return a `model_out_tbl` of both pmf and quantile forecasts constructed from the 
#'   input quantile forecasts with the following columns: `model_id`, `location`,
#'   `reference_date`, `horizon`, `target`, `target_end_date`, `output_type`, 
#'   `output_type_id`, `value`
#' @export
#'
#' @examples
#' 
#' library(distfromq)
#' library(ggforce)
#' library(gridExtra)
#' library(reshape2)
#' horizons_2324_new <- 0:3; categories <- c("large_increase", "increase", "stable", "decrease", "large_decrease")
#' count_rate_multiplier_2324_new <- matrix(c(c(2, 3, 4, 5), c(1, 1, 2, 2.5), c(-1, -1, -2, -2.5), c(-2, -3, -4, -5)), ncol=4)
#' category_rule_2324_new <- matrix(c(rep(10, 4), rep(10, 4), rep(-10, 4), rep(-10, 4)), ncol=4)

get_pmf_forecasts_from_quantile <- function(quantile_forecasts, locations_df, truth_df, categories, horizons=1, count_rate_multiplier, category_rule, target_name="wk flu hosp rate change", save_path=NULL) {
  num_cat = length(categories)
  
  truth_df_all <- truth_df |>
    dplyr::rename(target_end_date = time_value) |>
    dplyr::ungroup() |>
    dplyr::left_join(truth_df, by=c("target_end_date"="time_value", "geo_value", "value")) |>
    dplyr::inner_join(location_data, by = c("geo_value"))  |>
    dplyr::mutate(model_id="Observed Data", target_variable=target_name, .before=1)
  
  truth_df_temp <- truth_df_all 
  truth_df_all <- NULL
  for (j in 1:length(horizons)) {
    truth_df_temp[["horizon"]] <- horizons[j]
    for (i in 1:(num_cat-1)) {
      truth_df_temp[[paste("crit", i, sep="")]] <-
        ifelse(
          abs(truth_df_temp[["population"]]*count_rate_multiplier[j,i]/100000) < abs(category_rule[j,i]), 
          truth_df_temp[["value"]] + category_rule[j,i], 
          truth_df_temp[["value"]] + 
            round(truth_df_temp[["population"]]*count_rate_multiplier[j,i]/100000, digits=0)
        )
    }
    truth_df_all <- rbind(truth_df_all, truth_df_temp)
  }

  truth_df_all <- truth_df_all |>
    dplyr::select(model_id, location_name, location, value, target_end_date, horizon,target_variable, population, crit1:ncol(truth_df_all)) |>
    dplyr::filter(!is.na(value))

  train_forecasts <- truth_df_filtered <- truth_df_all |>
    dplyr::select(location_name, location, horizon, target_end_date, target_variable, population, crit1:ncol(truth_df_all)) |>
    dplyr::mutate(date=target_end_date+weeks(1), target_end_date = date+weeks(horizon), .before = 3) 
  

  # list of locations
  the_locations <- truth_df_filtered |>
    dplyr::distinct(location, .keep_all=TRUE) |>
    pull(location) #states, us and territories

  # extract log pdf and cdf values for training set forecasts
  # we add a little noise to the value column so that there is a density to
  # work with in case the forecaster had a point mass anywhere
  quantile_forecasts_adjusted <- quantile_forecasts |>
    dplyr::mutate(
      reference_date=as.Date(reference_date),
      value = rnorm(n = nrow(quantile_forecasts), mean = value, sd = 0.1)
    ) 
    
  truth_df_filtered <- truth_df_filtered |>
    dplyr::inner_join(
      quantile_forecasts_adjusted,
      by = c("date"="reference_date", "horizon", "target_end_date", "location")
    ) 

  train_forecasts <- truth_df_filtered |>
    dplyr::distinct(model_id, location, location_name, date, horizon, target_variable, .keep_all=TRUE) |>
    dplyr::select(-target, -output_type,-output_type_id,-value)
    
  for (i in 1:(num_cat-1)) {
    truth_df_filtered[["crit_current"]] <- truth_df_filtered[[paste0("crit", i, sep="")]] 
    train_temp <- truth_df_filtered |>
      dplyr::group_by(model_id, date, location, horizon, target, target_end_date) |>
      dplyr::summarize(
        cdf_crit_current = distfromq::make_p_fn(
          ps = output_type_id,
          qs = value)(unique(crit_current), log = FALSE)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-c(target, target_end_date))
    train_forecasts[[paste0("cdf_crit", i)]] <- train_temp[["cdf_crit_current"]]
  }
  
  #calculate percentages, correcting for negative numbers
  exp_forecast <- train_forecasts |>
    dplyr::ungroup() |>
    dplyr::rename(reference_date=date, target=target_variable) |>
    dplyr::mutate(cdf_crit0=1, .before=cdf_crit1)

  exp_forecast[[paste0("cdf_crit", num_cat)]] <- exp_forecast[[paste0("crit", num_cat)]] <- 0
  cdf_crit_sum <- 0
  for (i in 1:(num_cat)) {
    if (cdf_crit_sum < 1) {
      exp_forecast[[categories[i]]] <- exp_forecast[[paste0("cdf_crit", i-1)]] - ifelse(exp_forecast[[paste0("crit", i)]] > 0, exp_forecast[[paste0("cdf_crit", i)]], 0)
    } else {
      exp_forecast[[categories[i]]] <- 0
    }
   cdf_crit_sum <- cdf_crit_sum + mean(exp_forecast[[categories[i]]])
  }

  exp_forecast <- exp_forecast |>
    dplyr::select(model_id,reference_date,location,location_name, horizon, all_of(categories))
  

  #transpose data_frame to format for submission
  exp_t = melt(
    exp_forecast,
    id.vars = c("model_id","reference_date","location","location_name","horizon"),
    measure.vars = categories,
    variable.name="output_type_id",
    value.name="value"
  )
  exp_t <- exp_t |>
    dplyr::mutate(target=target_name, output_type="pmf") |>
    dplyr::select(model_id, reference_date, horizon, target, location, location_name, output_type, output_type_id, value)

  output_forecasts <- exp_t |>
    dplyr::select(-location_name) |>
    dplyr::mutate(
      output_type_id=as.character(output_type_id),
      target_end_date=reference_date+weeks(horizon)
    ) |>
    dplyr::bind_rows(mutate(quantile_forecasts, output_type_id=as.character(output_type_id)))|>
    dplyr::filter(location %in% pull(locations_df, location)) |>
    dplyr::select(reference_date, horizon, target, target_end_date, location, output_type, output_type_id, value)

  if (!is.null(save_path)) {
    readr::write_csv(output_forecasts, save_path)
  }

  return (output_forecasts)
}
