library(distfromq)
library(hubUtils)
library(dplyr)
library(tidyr)

location_data <- data.frame(
  stringsAsFactors = FALSE,
  geo_value = "lc",
  location = "111",
  population = 1)
  
truth_data <- data.frame(
  stringsAsFactors = FALSE,
  geo_value = "lc",
  time_value = as.Date("2021-12-18"),
  target = "inc death", #assume the target is the same
  value = 0)
  
quantile_inputs <- data.frame(
  stringsAsFactors = FALSE,
  model_id = "quantile-model",
  location = "111",
  reference_date = as.Date("2021-12-25"),
  horizon = 1,
  target = "inc death",
  target_end_date = as.Date("2022-01-01"),
  output_type = "quantile",
  output_type_id = rep(NA, 21),
  value = NA_real_)

quantile_values <- runif(21, 0, 10) |> sort()
output_prob <- seq(from=0, to=1, by=0.05)
quantile_inputs$value <- quantile_values
quantile_inputs$output_type_id <- output_prob

horizons <- 1 
categories <- c("large_increase", "increase", "stable", "decrease", "large_decrease")
count_rate_multiplier <- matrix(c(9, 7, 3, 1), ncol=4)
category_rule <- matrix(c(9, 7, 3, 1), ncol=4)
criteria <- c(9, 7, 3, 1) # population * count_rate_multiplier

test_that("category probabilities sum to 1", {
  
  pmf_output <- get_pmf_forecasts_from_quantile(quantile_inputs, location_data, truth_data, categories, horizons=1, count_rate_multiplier, category_rule, target_name="death rate change") 

  pmf_output |>
    dplyr::filter(output_type == "pmf") |>
    dplyr::group_by(model_id, reference_date, horizon, target, target_end_date, location) |>
    dplyr::summarize(prob_sum = sum(value)) |>
    dplyr::distinct(prob_sum) |>
    dplyr::pull(prob_sum) |>
    expect_equal(1)
})

#can base this on quantile forecasts where the quantiles are from a known normal distribution, and truth/target data constructed so that the bin endpoints are easy/known quantities. this makes it so you can directly compute the probabilities using pnorm(upper, ...) - pnorm(lower, ...).

test_that("category probabilities are correctly calculated", {
  # Quantile forecasts have quantiles from distribution F = N(5, 5)
  quantile_values <- seq(from = 0, to = 10, by = 0.5) # expected
  output_prob <- stats::pnorm(quantile_values, mean = 5, 5)
  quantile_inputs$value <- quantile_values
  quantile_inputs$output_type_id <- output_prob

  pmf_expected <- expand.grid(
    stringsAsFactors = FALSE,
    model_id = "quantile-model",
    reference_date = as.Date("2021-12-25"),
    horizon = 1,
    target = "death rate change",
    target_end_date = as.Date("2022-01-01"),
    location = "111",
    output_type = "pmf",
    output_type_id = categories,
    value = NA_real_)

  upper_prob <- 1; criteria <- c(criteria, -1e3)
  for (i in 1:length(categories)) {
    pmf_expected$value[pmf_expected$output_type_id == categories[i]] <-
      upper_prob - pnorm(criteria[i], 5, 5, lower.tail = T)
    upper_prob <- pnorm(criteria[i], 5, 5, lower.tail = T)
  }
  attr(pmf_expected, 'out.attrs') <- NULL
  
  pmf_actual <- get_pmf_forecasts_from_quantile(quantile_inputs, location_data, truth_data, categories, horizons=1, count_rate_multiplier, category_rule, target_name="death rate change") 

  expect_equal(rbind(pmf_expected, quantile_inputs) |> as_model_out_tbl(),
               pmf_actual|> as_model_out_tbl(),
               tolerance=1e-3)
})



# Check `quantile_forecasts` is a `model_out_tbl` with columns `model_id`, `reference_date`, `horizon`, `target`, `target_end_date`, `output_type`, `output_type_id`, `value`
# Check `locations_df` is a data frame of locations to forecast for with columns `geo_value`, `location`, `location_name`, `population`
  # Should we have NULL default with `hubUtils` locations datasets? They don't have a population column though
# Check truth_df is a data frame with columns `geo_value`, `time_value`, `value`
# Check `categories` has length > 1?
# Check `horizons` is numeric; also check the default of horizons = 1 works
# Check `count_rate_multiplier` is a numeric matrix of dimension length (horizons) x length (categories)
# Check `category_rule` is a numeric matrix of dimension length (horizons) x length (categories); add default value or way to not use?

# Test function works with old format of forecast_date (we require hubverse format otherwise)
# Test function works with multiple models in `quantile_forecasts`
# Test function works with multiple reference dates in `quantile_forecasts`

# Options for other task_id variables?