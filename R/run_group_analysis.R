#' Run Full Group Analysis Pipeline
#'
#' This function iterates over a list of group variables, performs the standard analysis
#' for each group, adds labels, and pivots the results. For the first group in the list
#' (usually 'Overall'), it assigns a unique UUID to each result row. For all subsequent
#' group variables, it matches the results back to the original UUIDs using \code{analysis_var},
#' \code{analysis_type}, and \code{analysis_var_value}. This avoids the need to repeatedly
#' read and write to an Excel file during the loop, producing one massive dataset spanning all groups.
#'
#' @param dataset The survey dataset.
#' @param dap Data Analysis Plan as a data frame.
#' @param group_variables A character vector of group variables to iterate over. Defaults to c("Overall").
#' @param tool_survey The Kobo tool survey sheet.
#' @param tool_choices The Kobo tool choices sheet.
#' @param weight_column The column name for survey weights. Defaults to "weights".
#' @param strata_column The column name for survey strata. Defaults to "sample_location".
#' @param value_columns Columns to extract from results. Defaults to c("stat", "n", "n_total").
#' @param extra_columns Extra columns to keep. Defaults to NULL.
#' @return A list containing \code{combined_results} (the merged dataset spanning all group variables) and \code{uuid_table} (the baseline UUID mappings).
#' @export
#' @importFrom dplyr mutate filter all_of left_join select relocate n across pull
#' @importFrom uuid UUIDgenerate
#' @importFrom analysistools create_analysis
#' @importFrom srvyr as_survey_design
#' @importFrom presentresults create_label_dictionary add_label_columns_to_results_table
run_group_analysis_pipeline <- function(
  dataset,
  dap,
  group_variables = c("Overall"),
  tool_survey,
  tool_choices,
  weight_column = "weights",
  strata_column = "sample_location",
  value_columns = c("stat", "n", "n_total"),
  extra_columns = NULL
) {
  combined_analysis <- NULL
  uuid_table <- NULL

  for (i in seq_along(group_variables)) {
    var <- group_variables[i]
    is_first <- (i == 1)

    cat(sprintf(
      "\nProcessing group variable (%d/%d): %s\n",
      i,
      length(group_variables),
      var
    ))

    # 1. Format DAP for the current group
    my_loa <- dap %>%
      dplyr::mutate(group_var = ifelse(var == "Overall", group_var, var)) %>%
      dplyr::filter(analysis_var != var) %>%
      dplyr::filter(analysis_var %in% colnames(dataset))

    # 2. Convert required columns to numeric based on mean/median flag in DAP
    numeric_cols <- my_loa %>%
      dplyr::filter(tolower(analysis_type) %in% c("mean", "median")) %>%
      dplyr::pull(analysis_var) %>%
      intersect(names(dataset))

    if (length(numeric_cols) > 0) {
      dataset <- dataset %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(numeric_cols), as.numeric))
    }

    # 3. Create survey design
    my_design <- srvyr::as_survey_design(
      dataset,
      weights = dplyr::all_of(weight_column),
      strata = dplyr::all_of(strata_column)
    )

    # 4. Run analysis
    my_results <- analysistools::create_analysis(
      my_design,
      loa = my_loa,
      sm_separator = "."
    )

    # 5. Merge results with DAP
    join_cols <- intersect(names(my_results$results_table), names(my_loa))
    if (length(join_cols) > 0) {
      my_results_table <- dplyr::left_join(
        my_results$results_table,
        my_loa,
        by = join_cols
      )
    } else {
      my_results_table <- my_results$results_table
    }

    # 6. Create dictionary and attach labels
    dictionary <- presentresults::create_label_dictionary(
      kobo_survey_sheet = tool_survey,
      kobo_choices_sheet = tool_choices,
      label_column = "label",
      analysis_type_dictionary = NULL,
      results_table = my_results_table
    )

    label_results <- presentresults::add_label_columns_to_results_table(
      results_table = my_results_table,
      dictionary = dictionary
    )

    # 7. Pivot results using custom function
    if (!is.null(extra_columns)) {
      valid_extra_columns <- intersect(extra_columns, names(label_results))
    } else {
      valid_extra_columns <- NULL
    }

    label_results_final <- my_create_table_variable_x_group(
      label_results,
      analysis_key = "label_analysis_key",
      value_columns = value_columns,
      extra_columns = valid_extra_columns
    ) %>%
      dplyr::mutate(
        analysis_var_value = ifelse(
          analysis_var_value == "NA" | is.na(analysis_var_value),
          paste0("var_", analysis_var),
          analysis_var_value
        )
      )

    # 8. Manage UUIDs across groupings
    if (is_first) {
      # Generate UUID for the baseline dataset
      if (!"uuid" %in% names(label_results_final)) {
        label_results_final <- label_results_final %>%
          dplyr::mutate(uuid = uuid::UUIDgenerate(n = dplyr::n())) %>%
          dplyr::relocate(uuid)
      }

      uuid_table <- label_results_final %>%
        dplyr::select(uuid, analysis_type, analysis_var, analysis_var_value)

      combined_analysis <- label_results_final
    } else {
      # For subsequent groups, align with existing UUIDs to enforce consistent rows
      label_results_temp <- dplyr::left_join(
        uuid_table,
        label_results_final,
        by = c("analysis_type", "analysis_var", "analysis_var_value")
      ) %>%
        dplyr::relocate(uuid)

      # Horizontally join this new block to the overarching combined analysis
      join_keys <- intersect(
        names(combined_analysis),
        names(label_results_temp)
      )
      combined_analysis <- dplyr::left_join(
        combined_analysis,
        label_results_temp,
        by = join_keys
      )
    }
  }

  if ("uuid" %in% names(combined_analysis)) {
    combined_analysis$uuid <- NULL
  }

  return(list(
    combined_results = combined_analysis,
    uuid_table = uuid_table
  ))
}

#' Create Table Variable x Group
#'
#' function to add sector and indicator to the table for output
#'
#' @param results_table the results
#' @param analysis_key the key
#' @param value_columns columns
#' @param extra_columns optional columns
#' @param list_for_excel internal use
#' @return A pivotted dataframe
#' @export
my_create_table_variable_x_group <- function(
  results_table,
  analysis_key = "analysis_key",
  value_columns = c("stat", "stat_low", "stat_upp"),
  extra_columns = NULL, # User can specify extra columns like c("sector", "indicator")
  list_for_excel = FALSE
) {
  analysistools::verify_analysis_key(results_table[[analysis_key]])

  # Ensure extra_columns is a character vector and exists in results_table
  if (!is.null(extra_columns)) {
    extra_columns <- intersect(extra_columns, colnames(results_table))
  } else {
    extra_columns <- character(0) # Empty if not provided
  }

  # Create the analysis key table
  analysis_key_table <- results_table |>
    analysistools::create_analysis_key_table(
      analysis_key_column = analysis_key
    ) |>
    analysistools::unite_variables()

  # Join the analysis key table back to the results table and select necessary columns
  results_table <- results_table[, c(
    analysis_key,
    value_columns,
    extra_columns
  )] |>
    dplyr::left_join(analysis_key_table, by = analysis_key)

  if (list_for_excel) {
    table_to_return <- results_table %>%
      dplyr::group_by(group_var) %>%
      dplyr::group_split() %>%
      purrr::map(
        ~ tidyr::pivot_wider(
          .,
          id_cols = c(
            analysis_type,
            analysis_var,
            analysis_var_value,
            dplyr::all_of(extra_columns)
          ),
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
        id_cols = c(
          analysis_type,
          analysis_var,
          analysis_var_value,
          dplyr::all_of(extra_columns)
        ),
        names_from = group_var_value,
        values_from = dplyr::all_of(value_columns),
        names_vary = "slowest"
      ) %>%
      dplyr::relocate(
        dplyr::all_of(extra_columns),
        .before = dplyr::everything()
      ) # Ensure extra columns appear first

    return(table_to_return)
  }
}
