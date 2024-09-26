test_that("get_flusight_bin_endpoints works, 2023/24", {
  # definitions at
  # https://github.com/cdcepi/FluSight-forecast-hub/tree/main/model-output#rate-trend-forecast-specifications
  #
  # our strategy is to:
  # - construct data that should fall into known categories
  #   (with every horizon/category combination, and both criteria for stable)
  # - compute bins and apply them to the data
  # - check that we got the right answers
  location_meta <- readr::read_csv(
    file = testthat::test_path("fixtures", "location_meta_24.csv")
  ) |>
    dplyr::mutate(
      pop100k = .data[["population"]] / 100000
    )

  # locations for testing "stable", "increase" and "decrease" thresholds are:
  # 56 = Wyoming, pop100k = 5.78
  # 02 = Alaska, pop100k = 7.11 and 11 = District of Columbia, pop100k = 6.69
  # these states trigger the "minimum count change at least 10" rule
  locs <- c("US", "56", "02", "11", "05", "06")

  # create data
  # our reference date will be 2023-10-21.
  # changes are relative to 2023-10-14.
  # 2023-10-07 is throw-away, to make sure we grab the right "relative to" date
  target_data <- tidyr::expand_grid(
    location = locs,
    date = as.Date("2023-10-07") + seq(from = 0, by = 7, length.out = 6),
    value = NA
  )

  expected_categories <- NULL

  # all expected category levels are "stable": rate change less than
  # 1, 1, 2, or 2.5 * population rate
  loc_pop100k <- location_meta$pop100k[location_meta$location == locs[1]]
  target_data$value[target_data$location == locs[1]] <- c(
    0,
    10000,
    10000 + floor(0.99 * loc_pop100k),
    10000 - floor(0.99 * loc_pop100k),
    10000 + floor(1.99 * loc_pop100k),
    10000 - floor(2.49 * loc_pop100k)
  )
  expected_categories <- dplyr::bind_rows(
    expected_categories,
    target_data |>
      dplyr::filter(
        .data[["location"]] == locs[1],
        .data[["date"]] >= "2023-10-21"
      ) |>
      dplyr::mutate(
        horizon = as.integer((.data[["date"]] - as.Date("2023-10-21")) / 7),
        output_type_id = "stable"
      )
  )

  # all expected category levels are "stable": count change less than 10
  target_data$value[target_data$location == locs[2]] <- c(0, 300, 304, 297, 309, 291)
  expected_categories <- dplyr::bind_rows(
    expected_categories,
    target_data |>
      dplyr::filter(
        .data[["location"]] == locs[2],
        .data[["date"]] >= "2023-10-21"
      ) |>
      dplyr::mutate(
        horizon = as.integer((.data[["date"]] - as.Date("2023-10-21")) / 7),
        output_type_id = "stable"
      )
  )

  # all expected category levels are "increase": count change >= 10,
  # horizon 0: 1 <= rate change < 2
  # horizon 1: 1 <= rate change < 3
  # horizon 2: 2 <= rate change < 4
  # horizon 3: 2.5 <= rate change < 5
  # note, loc_pop100k for this location is 7.11 < 10
  loc_pop100k <- location_meta$pop100k[location_meta$location == locs[3]]
  target_data$value[target_data$location == locs[3]] <- c(
    0,
    10000,
    10000 + 10,
    10000 + floor(2.99 * loc_pop100k),
    10000 + floor(3.99 * loc_pop100k),
    10000 + ceiling(2.51 * loc_pop100k)
  )
  expected_categories <- dplyr::bind_rows(
    expected_categories,
    target_data |>
      dplyr::filter(
        .data[["location"]] == locs[3],
        .data[["date"]] >= "2023-10-21"
      ) |>
      dplyr::mutate(
        horizon = as.integer((.data[["date"]] - as.Date("2023-10-21")) / 7),
        output_type_id = "increase"
      )
  )

  # all expected category levels are "decrease": count change <= -10,
  # horizon 0: -1 >= rate change > -2
  # horizon 1: -1 >= rate change > -3
  # horizon 2: -2 >= rate change > -4
  # horizon 3: -2.5 >= rate change > -5
  # note, loc_pop100k for this location is 73.4 >= 10
  loc_pop100k <- location_meta$pop100k[location_meta$location == locs[4]]
  target_data$value[target_data$location == locs[4]] <- c(
    0,
    10000,
    10000 - 10,
    10000 - floor(2.99 * loc_pop100k),
    10000 - floor(3.99 * loc_pop100k),
    10000 - ceiling(2.51 * loc_pop100k)
  )
  expected_categories <- dplyr::bind_rows(
    expected_categories,
    target_data |>
      dplyr::filter(
        .data[["location"]] == locs[4],
        .data[["date"]] >= "2023-10-21"
      ) |>
      dplyr::mutate(
        horizon = as.integer((.data[["date"]] - as.Date("2023-10-21")) / 7),
        output_type_id = "decrease"
      )
  )

  # all expected category levels are "large increase": count change >= 10,
  # horizon 0: 2 <= rate change
  # horizon 1: 3 <= rate change
  # horizon 2: 4 <= rate change
  # horizon 3: 5 <= rate change
  loc_pop100k <- location_meta$pop100k[location_meta$location == locs[5]]
  target_data$value[target_data$location == locs[5]] <- c(
    0,
    10000,
    10000 + max(10, ceiling(2 * loc_pop100k)),
    10000 + max(10, ceiling(3 * loc_pop100k)),
    10000 + max(10, ceiling(4 * loc_pop100k)),
    10000 + max(10, ceiling(5 * loc_pop100k))
  )

  expected_categories <- dplyr::bind_rows(
    expected_categories,
    target_data |>
      dplyr::filter(
        .data[["location"]] == locs[5],
        .data[["date"]] >= "2023-10-21"
      ) |>
      dplyr::mutate(
        horizon = as.integer((.data[["date"]] - as.Date("2023-10-21")) / 7),
        output_type_id = "large_increase"
      )
  )

  # all expected category levels are "large decrease": count change <= -10,
  # horizon 0: -2 >= rate change
  # horizon 1: -3 >= rate change
  # horizon 2: -4 >= rate change
  # horizon 3: -5 >= rate change
  loc_pop100k <- location_meta$pop100k[location_meta$location == locs[6]]
  target_data$value[target_data$location == locs[6]] <- c(
    0,
    10000,
    10000 - max(10, ceiling(2 * loc_pop100k)),
    10000 - max(10, ceiling(3 * loc_pop100k)),
    10000 - max(10, ceiling(4 * loc_pop100k)),
    10000 - max(10, ceiling(5 * loc_pop100k))
  )

  expected_categories <- dplyr::bind_rows(
    expected_categories,
    target_data |>
      dplyr::filter(
        .data[["location"]] == locs[6],
        .data[["date"]] >= "2023-10-21"
      ) |>
      dplyr::mutate(
        horizon = as.integer((.data[["date"]] - as.Date("2023-10-21")) / 7),
        output_type_id = "large_decrease"
      )
  )

  bin_endpoints <- get_flusight_bin_endpoints(
    target_ts = target_data |>
      dplyr::filter(
        .data[["date"]] < "2023-10-21"
      ),
    location_meta = location_meta,
    season = "2023/24"
  )

  actual_categories <- bin_endpoints |>
    dplyr::mutate(
      reference_date = as.Date("2023-10-21"),
      target_end_date = as.Date("2023-10-21") + 7 * .data[["horizon"]]
    ) |>
    dplyr::left_join(
      target_data,
      by = c("location", "target_end_date" = "date")
    ) |>
    dplyr::filter(
      .data[["lower"]] < .data[["value"]],
      .data[["value"]] <= .data[["upper"]]
    )

  mismatched_categorizations <- expected_categories |>
    dplyr::left_join(
      actual_categories,
      by = c("location", "date" = "reference_date", "horizon")
    ) |>
    dplyr::filter(output_type_id.x != output_type_id.y)

  # expect no mismatches!
  expect_equal(nrow(mismatched_categorizations), 0L)
})
