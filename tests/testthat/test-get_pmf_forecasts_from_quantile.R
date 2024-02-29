library(distfromq)
library(hubUtils)
library(dplyr)
library(tidyr)

location_data <- data.frame(
  stringsAsFactors = FALSE,
  geo_value = c("aa", "bb"),
  location = c("111", "222"),
  population = c(100000, 110000))
  
truth_data <- expand.grid(
  stringsAsFactors = FALSE,
  geo_value = c("aa", "bb"),
  time_value = c(as.Date("2021-12-11"), as.Date("2021-12-18")),
  target = "inc death", #assume the target is the same
  value = NA)
truth_data$value = c(6, 7, 5, 6)
  
quantile_inputs <- data.frame(
  stringsAsFactors = FALSE,
  model_id = "quantile-model",
  location = "111",
  reference_date = as.Date("2021-12-25"),
  horizon = 1,
  target = "inc death",
  target_end_date = as.Date("2022-01-01"),
  output_type = "quantile",
  output_type_id = rep(NA, 5),
  value = NA_real_)

quantile_values <- runif(5, -5, 15) |> sort()
output_prob <- seq(from=0, to=1, by=0.25)
quantile_inputs$value <- quantile_values
quantile_inputs$output_type_id <- output_prob

horizons <- 1 
categories <- c("large_increase", "increase", "stable", "decrease", "large_decrease")
count_rate_multiplier <- matrix(c(4, 2, -2, -4), ncol=4)
category_rule <- matrix(c(4, 2, -2, -4), ncol=4)
criteria <- c(4, 2, -2, -4)

test_that("category probabilities sum to 1", {
  pmf_output <- get_pmf_forecasts_from_quantile(quantile_inputs, location_data, truth_data, categories, horizons=1, count_rate_multiplier, category_rule, target_name="death rate change") 

  pmf_output |>
    dplyr::filter(output_type == "pmf") |>
    dplyr::group_by(model_id, reference_date, horizon, target, target_end_date, location) |>
    dplyr::summarize(prob_sum = sum(value)) |>
    dplyr::ungroup() |>
    dplyr::distinct(prob_sum) |>
    dplyr::pull(prob_sum) |>
    expect_equal(1)
})

test_that("category probabilities are correctly calculated", {
  # Quantile forecasts have quantiles from distribution F = N(5, 5)
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
  
  quantile_values <- seq(from = 0, to = 10, by = 0.5)
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
  truth_value <- truth_data$value[3]
  for (i in 1:length(categories)) {
    pmf_expected$value[pmf_expected$output_type_id == categories[i]] <-
      upper_prob - pnorm(criteria[i] + truth_value, 5, 5, lower.tail = T)
    upper_prob <- pnorm(criteria[i] + truth_value, 5, 5, lower.tail = T)
  }
  attr(pmf_expected, 'out.attrs') <- NULL
  
  pmf_actual <- get_pmf_forecasts_from_quantile(quantile_inputs, location_data, truth_data, categories, horizons=1, count_rate_multiplier, category_rule, target_name="death rate change") 

  expect_equal(rbind(pmf_expected, quantile_inputs) |> hubUtils::as_model_out_tbl(),
               pmf_actual|> hubUtils::as_model_out_tbl(),
               tolerance=1e-3)
})

