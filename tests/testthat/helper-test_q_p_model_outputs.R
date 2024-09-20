test_q_p_model_outputs <- function(bin_endpoints) {
  bins_by <- colnames(bin_endpoints)
  bins_by <- bins_by[!bins_by %in% c("output_type_id", "lower", "upper")]

  model_outputs <- expand.grid(
    model_id = c("model1", "model2"),
    location = c("a", "b"),
    age_group = c("0-18", "19-65", "66+"),
    output_type = "quantile",
    output_type_id = seq(from = 0.1, to = 0.9, by = 0.1),
    stringsAsFactors = FALSE
  )

  pred_moments <- expand.grid(
    model_id = c("model1", "model2"),
    location = c("a", "b"),
    age_group = c("0-18", "19-65", "66+"),
    stringsAsFactors = FALSE
  )
  pred_moments$mean <- 10 * seq_len(nrow(pred_moments))
  pred_moments$sd <- 10 / rev(seq_len(nrow(pred_moments)))

  q_model_outputs <- model_outputs |>
    dplyr::left_join(
      pred_moments,
      by = c("model_id", "location", "age_group")
    ) |>
    dplyr::mutate(
      output_type = "quantile",
      value = qnorm(
        p = output_type_id,
        mean = mean,
        sd = sd
      )
    ) |>
    dplyr::select(model_id, location, age_group, output_type, output_type_id, value)

  if (length(bins_by) > 0) {
    p_model_outputs <- pred_moments |>
      dplyr::full_join(
        bin_endpoints,
        by = bins_by,
        relationship = "many-to-many"
      )
  } else {
    p_model_outputs <- pred_moments |>
      dplyr::cross_join(
        bin_endpoints
      )
  }
  p_model_outputs <- p_model_outputs |>
    dplyr::mutate(
      output_type = "pmf",
      value = pnorm(upper, mean = mean, sd = sd) - pnorm(lower, mean = mean, sd = sd)
    ) |>
    dplyr::select(model_id, location, age_group, output_type, output_type_id, value)

  return(list(p_model_outputs = p_model_outputs, q_model_outputs = q_model_outputs))
}
