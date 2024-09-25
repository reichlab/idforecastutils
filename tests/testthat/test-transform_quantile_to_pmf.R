test_that("transform_quantile_to_pmf works, shared bin endpoints", {
  bin_endpoints <- data.frame(
    output_type_id = c("low", "med", "high"),
    stringsAsFactors = FALSE
  )
  bin_endpoints$lower <- c(-Inf, 40, 80)
  bin_endpoints$upper <- c(40, 80, Inf)

  test_data <- test_q_p_model_outputs(bin_endpoints)
  q_model_outputs <- test_data$q_model_outputs
  expected_p_model_outputs <- test_data$p_model_outputs

  actual_p_model_outputs <- transform_quantile_to_pmf(q_model_outputs, bin_endpoints)

  # same column names, number of rows, and probability values
  expect_equal(colnames(actual_p_model_outputs), colnames(expected_p_model_outputs))
  expect_equal(nrow(actual_p_model_outputs), nrow(expected_p_model_outputs))
  merged_p_model_outputs <- dplyr::full_join(
    actual_p_model_outputs, expected_p_model_outputs,
    by = c("model_id", "location", "age_group", "output_type", "output_type_id")
  )
  expect_equal(nrow(actual_p_model_outputs), nrow(merged_p_model_outputs))
  expect_equal(merged_p_model_outputs$value.x, merged_p_model_outputs$value.y, tolerance = 1e-12)
})


test_that("transform_quantile_to_pmf works, model_out_tbl contains extra output types", {
  bin_endpoints <- data.frame(
    output_type_id = c("low", "med", "high"),
    stringsAsFactors = FALSE
  )
  bin_endpoints$lower <- c(-Inf, 40, 80)
  bin_endpoints$upper <- c(40, 80, Inf)

  test_data <- test_q_p_model_outputs(bin_endpoints)
  q_model_outputs <- test_data$q_model_outputs
  expected_p_model_outputs <- test_data$p_model_outputs

  test_model_outputs <- dplyr::bind_rows(
    q_model_outputs,
    q_model_outputs |>
      dplyr::filter(
        output_type_id == 0.5
      ) |>
      dplyr::mutate(
        output_type_id = NA_real_,
        output_type = "median"
      )
  )
  actual_p_model_outputs <- transform_quantile_to_pmf(test_model_outputs, bin_endpoints)

  # same column names, number of rows, and probability values
  expect_equal(colnames(actual_p_model_outputs), colnames(expected_p_model_outputs))
  expect_equal(nrow(actual_p_model_outputs), nrow(expected_p_model_outputs))
  merged_p_model_outputs <- dplyr::full_join(
    actual_p_model_outputs, expected_p_model_outputs,
    by = c("model_id", "location", "age_group", "output_type", "output_type_id")
  )
  expect_equal(nrow(actual_p_model_outputs), nrow(merged_p_model_outputs))
  expect_equal(merged_p_model_outputs$value.x, merged_p_model_outputs$value.y, tolerance = 1e-12)
})


test_that("transform_quantile_to_pmf works, bin endpoints depend on task id variable", {
  bin_endpoints <- expand.grid(
    output_type_id = c("low", "med", "high"),
    age_group = c("0-18", "19-65", "66+"),
    stringsAsFactors = FALSE
  )
  bin_endpoints$lower <- c(-Inf, 20, 40, -Inf, 40, 80, -Inf, 50, 100)
  bin_endpoints$upper <- c(20, 40, Inf, 40, 80, Inf, 50, 100, Inf)

  test_data <- test_q_p_model_outputs(bin_endpoints)
  q_model_outputs <- test_data$q_model_outputs
  expected_p_model_outputs <- test_data$p_model_outputs

  actual_p_model_outputs <- transform_quantile_to_pmf(q_model_outputs, bin_endpoints)

  # same column names, number of rows, and probability values
  expect_equal(colnames(actual_p_model_outputs), colnames(expected_p_model_outputs))
  expect_equal(nrow(actual_p_model_outputs), nrow(expected_p_model_outputs))
  merged_p_model_outputs <- dplyr::full_join(
    actual_p_model_outputs, expected_p_model_outputs,
    by = c("model_id", "location", "age_group", "output_type", "output_type_id")
  )
  expect_equal(nrow(actual_p_model_outputs), nrow(merged_p_model_outputs))
  expect_equal(merged_p_model_outputs$value.x, merged_p_model_outputs$value.y, tolerance = 1e-12)
})


test_that("transform_quantile_to_pmf works, bin endpoints depend on task id variable, extra bin endpoints", {
  bin_endpoints <- expand.grid(
    output_type_id = c("low", "med", "high"),
    age_group = c("0-18", "19-65", "66+"),
    stringsAsFactors = FALSE
  )
  bin_endpoints$lower <- c(-Inf, 20, 40, -Inf, 40, 80, -Inf, 50, 100)
  bin_endpoints$upper <- c(20, 40, Inf, 40, 80, Inf, 50, 100, Inf)

  test_data <- test_q_p_model_outputs(bin_endpoints)
  q_model_outputs <- test_data$q_model_outputs
  expected_p_model_outputs <- test_data$p_model_outputs

  bin_endpoints <- dplyr::bind_rows(
    bin_endpoints,
    bin_endpoints |>
      dplyr::filter(age_group == "66+") |>
      dplyr::mutate(age_group = "really_old")
  )

  actual_p_model_outputs <- transform_quantile_to_pmf(q_model_outputs, bin_endpoints)

  # same column names, number of rows, and probability values
  expect_equal(colnames(actual_p_model_outputs), colnames(expected_p_model_outputs))
  expect_equal(nrow(actual_p_model_outputs), nrow(expected_p_model_outputs))
  merged_p_model_outputs <- dplyr::full_join(
    actual_p_model_outputs, expected_p_model_outputs,
    by = c("model_id", "location", "age_group", "output_type", "output_type_id")
  )
  expect_equal(nrow(actual_p_model_outputs), nrow(merged_p_model_outputs))
  expect_equal(merged_p_model_outputs$value.x, merged_p_model_outputs$value.y, tolerance = 1e-12)
})


test_that("transform_quantile_to_pmf errors if model_out_tbl is not convertible to object of class model_out_tbl", {
  bin_endpoints <- expand.grid(
    output_type_id = c("low", "med", "high"),
    age_group = c("0-18", "19-65", "66+"),
    stringsAsFactors = FALSE
  )
  bin_endpoints$lower <- c(-Inf, 20, 40, -Inf, 40, 80, -Inf, 50, 100)
  bin_endpoints$upper <- c(20, 40, Inf, 40, 80, Inf, 50, 100, Inf)

  test_data <- test_q_p_model_outputs(bin_endpoints)
  q_model_outputs <- test_data$q_model_outputs
  expected_p_model_outputs <- test_data$p_model_outputs

  expect_error(
    suppressMessages(transform_quantile_to_pmf(q_model_outputs |> select(-model_id), bin_endpoints)),
    regexp = "Cannot create `model_id` column."
  )
})


test_that("transform_quantile_to_pmf errors if model_out_tbl does not have quantile output type", {
  bin_endpoints <- expand.grid(
    output_type_id = c("low", "med", "high"),
    age_group = c("0-18", "19-65", "66+"),
    stringsAsFactors = FALSE
  )
  bin_endpoints$lower <- c(-Inf, 20, 40, -Inf, 40, 80, -Inf, 50, 100)
  bin_endpoints$upper <- c(20, 40, Inf, 40, 80, Inf, 50, 100, Inf)

  test_data <- test_q_p_model_outputs(bin_endpoints)
  q_model_outputs <- test_data$q_model_outputs
  expected_p_model_outputs <- test_data$p_model_outputs

  expect_error(
    suppressMessages(transform_quantile_to_pmf(q_model_outputs |> dplyr::mutate(output_type = "mean"))),
    regexp = "`model_out_tbl` must contain predictions with output type 'quantile'."
  )
})


test_that("transform_quantile_to_pmf errors if bin_endpoints is not a data frame", {
  bin_endpoints <- expand.grid(
    output_type_id = c("low", "med", "high"),
    age_group = c("0-18", "19-65", "66+"),
    stringsAsFactors = FALSE
  )
  bin_endpoints$lower <- c(-Inf, 20, 40, -Inf, 40, 80, -Inf, 50, 100)
  bin_endpoints$upper <- c(20, 40, Inf, 40, 80, Inf, 50, 100, Inf)

  test_data <- test_q_p_model_outputs(bin_endpoints)
  q_model_outputs <- test_data$q_model_outputs
  expected_p_model_outputs <- test_data$p_model_outputs

  expect_error(
    transform_quantile_to_pmf(q_model_outputs, bin_endpoints = "a"),
    regexp = "`bin_endpoints` must be a data frame."
  )
})


test_that("transform_quantile_to_pmf errors if bin_endpoints has incorrect columns", {
  bin_endpoints <- expand.grid(
    output_type_id = c("low", "med", "high"),
    age_group = c("0-18", "19-65", "66+"),
    stringsAsFactors = FALSE
  )
  bin_endpoints$lower <- c(-Inf, 20, 40, -Inf, 40, 80, -Inf, 50, 100)
  bin_endpoints$upper <- c(20, 40, Inf, 40, 80, Inf, 50, 100, Inf)

  test_data <- test_q_p_model_outputs(bin_endpoints)
  q_model_outputs <- test_data$q_model_outputs
  expected_p_model_outputs <- test_data$p_model_outputs

  expect_error(
    transform_quantile_to_pmf(q_model_outputs, bin_endpoints |> select(-output_type_id)),
    regexp = "`bin_endpoints` was missing 1 required column."
  )

  expect_error(
    transform_quantile_to_pmf(q_model_outputs, bin_endpoints |> select(-lower)),
    regexp = "`bin_endpoints` was missing 1 required column."
  )

  expect_error(
    transform_quantile_to_pmf(q_model_outputs, bin_endpoints |> rename(group_age = age_group)),
    regexp = "unexpected column name."
  )
})


test_that("transform_quantile_to_pmf errors if bin_endpoints has different output_type_ids in different groups", {
  bin_endpoints <- expand.grid(
    output_type_id = c("low", "med", "high"),
    age_group = c("0-18", "19-65", "66+"),
    stringsAsFactors = FALSE
  )
  bin_endpoints$lower <- c(-Inf, 20, 40, -Inf, 40, 80, -Inf, 50, 100)
  bin_endpoints$upper <- c(20, 40, Inf, 40, 80, Inf, 50, 100, Inf)

  test_data <- test_q_p_model_outputs(bin_endpoints)
  q_model_outputs <- test_data$q_model_outputs
  expected_p_model_outputs <- test_data$p_model_outputs

  bin_endpoints$output_type_id <- letters[seq_len(nrow(bin_endpoints))]

  expect_error(
    transform_quantile_to_pmf(q_model_outputs, bin_endpoints),
    regexp = "`bin_endpoints` must contain the same `output_type_id` values for each group defined by task id variables."
  )
})


test_that("transform_quantile_to_pmf errors if bin_endpoints has duplicate endpoints within groups", {
  bin_endpoints <- expand.grid(
    output_type_id = c("low", "med", "high"),
    age_group = c("0-18", "19-65", "66+"),
    stringsAsFactors = FALSE
  )
  bin_endpoints$lower <- c(-Inf, 20, 40, -Inf, 40, 80, -Inf, 50, 100)
  bin_endpoints$upper <- c(20, 40, Inf, 40, 80, Inf, 50, 100, Inf)

  test_data <- test_q_p_model_outputs(bin_endpoints)
  q_model_outputs <- test_data$q_model_outputs
  expected_p_model_outputs <- test_data$p_model_outputs

  expect_error(
    transform_quantile_to_pmf(q_model_outputs, bin_endpoints |> select(-age_group)),
    regexp = "`bin_endpoints` must not contain duplicated `output_type_id` values within any group defined by task id variables."
  )
})


test_that("transform_quantile_to_pmf errors if bin_endpoints has missing values in lower/upper", {
  bin_endpoints <- expand.grid(
    output_type_id = c("low", "med", "high"),
    age_group = c("0-18", "19-65", "66+"),
    stringsAsFactors = FALSE
  )
  bin_endpoints$lower <- c(-Inf, 20, 40, -Inf, 40, 80, -Inf, 50, 100)
  bin_endpoints$upper <- c(20, 40, Inf, 40, 80, Inf, 50, 100, Inf)

  test_data <- test_q_p_model_outputs(bin_endpoints)
  q_model_outputs <- test_data$q_model_outputs
  expected_p_model_outputs <- test_data$p_model_outputs

  be_l_miss <- bin_endpoints
  be_l_miss$lower[2] <- NA_real_

  be_u_miss <- bin_endpoints
  be_u_miss$upper[2] <- NA_real_

  expect_error(
    transform_quantile_to_pmf(q_model_outputs, be_l_miss),
    regexp = "`bin_endpoints` must not have any missing values in its `lower` and `upper` columns"
  )

  expect_error(
    transform_quantile_to_pmf(q_model_outputs, be_u_miss),
    regexp = "`bin_endpoints` must not have any missing values in its `lower` and `upper` columns"
  )
})


test_that("transform_quantile_to_pmf errors if bin_endpoints has missing values in lower/upper", {
  bin_endpoints <- expand.grid(
    output_type_id = c("low", "med", "high"),
    age_group = c("0-18", "19-65", "66+"),
    stringsAsFactors = FALSE
  )
  bin_endpoints$lower <- c(-Inf, 20, 40, -Inf, 40, 80, -Inf, 50, 100)
  bin_endpoints$upper <- c(20, 40, Inf, 40, 80, Inf, 50, 100, Inf)

  test_data <- test_q_p_model_outputs(bin_endpoints)
  q_model_outputs <- test_data$q_model_outputs
  expected_p_model_outputs <- test_data$p_model_outputs

  bin_endpoints$upper[2] <- 40.1

  expect_error(
    transform_quantile_to_pmf(q_model_outputs, bin_endpoints),
    regexp = "In `bin_endpoints`, the upper endpoint of each bin must equal the lower endpoint of the next bin."
  )
})
