#' Create Table Variable X Group
#'
#' This function extends the functionality of "create_table_variable_x_group" from analysis tools from
#' IMPACT HQ by providing an option of adding extra columns to the output
#'
#'
#' @param xxx Use all params from create_table_variable_x_group
#' @param extra_columns All the columns that you want included in the output
#' @return A new data frame with with all operations performed.
#' @examples
#' # examples
#' extraR_create_table_variable_x_group(
#'   analysis_key = "label_analysis_key",
#'   value_columns = c("stat", "n", "n_total"),
#'   extra_columns = c("number", "sector", "indicator", "subset")
#'   )
#' @export

extraR_create_table_variable_x_group <- function(
    results_table,
    analysis_key = "analysis_key",
    value_columns = c("stat", "stat_low", "stat_upp"),
    extra_columns = NULL,  # User can specify extra columns like c("sector", "indicator")
    list_for_excel = FALSE
) {
  analysistools::verify_analysis_key(results_table[[analysis_key]])

  # Ensure extra_columns is a character vector and exists in results_table
  if (!is.null(extra_columns)) {
    extra_columns <- intersect(extra_columns, colnames(results_table))
  } else {
    extra_columns <- character(0)  # Empty if not provided
  }

  # Create the analysis key table
  analysis_key_table <- results_table |>
    analysistools::create_analysis_key_table(analysis_key_column = analysis_key) |>
    analysistools::unite_variables()

  # Join the analysis key table back to the results table and select necessary columns
  results_table <- results_table[, c(analysis_key, value_columns, extra_columns)] |>
    dplyr::left_join(analysis_key_table, by = analysis_key)

  if (list_for_excel) {
    table_to_return <- results_table %>%
      dplyr::group_by(group_var) %>%
      dplyr::group_split() %>%
      purrr::map(
        ~ tidyr::pivot_wider(
          .,
          id_cols = c(analysis_type, analysis_var, analysis_var_value, dplyr::all_of(extra_columns)),
          names_from = group_var_value,
          values_from = dplyr::all_of(value_columns),
          names_vary = "slowest"
        )
      )

    group_names <- results_table %>%
      dplyr::group_by(group_var) %>%
      dplyr::group_keys() %>%
      dplyr::mutate(
        group_var = stringr::str_replace_na(group_var),
        group_var = stringr::str_replace_all(group_var, "/%", "")
      ) %>%
      dplyr::pull(group_var)

    table_to_return <- table_to_return %>% purrr::set_names(group_names)
    return(table_to_return)
  } else {
    table_to_return <- results_table %>%
      tidyr::pivot_wider(
        id_cols = c(analysis_type, analysis_var, analysis_var_value, dplyr::all_of(extra_columns)),
        names_from = group_var_value,
        values_from = dplyr::all_of(value_columns),
        names_vary = "slowest"
      ) %>%
      dplyr::relocate(dplyr::all_of(extra_columns), .before = everything())  # Ensure extra columns appear first

    return(table_to_return)
  }
}
