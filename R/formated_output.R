#' Insert Empty Rows into a Data Frame
#'
#' Internal helper function to insert empty rows between grouped elements.
#'
#' @param df The data frame.
#' @param col The column used to identify groups.
#' @return A modified dataset with empty rows separating distinct groups.
#' @keywords internal
add_empty_rows_between_groups <- function(df, col) {
  # Compute run-length encoding for the target column
  r <- rle(as.character(df[[col]]))

  # Initialize an empty data frame with the same columns as df
  new_df <- df[0, ]
  idx <- 1

  # Loop over each group and append the rows and an empty row (except after the last group)
  for (i in seq_along(r$lengths)) {
    group_length <- r$lengths[i]
    group_rows <- df[idx:(idx + group_length - 1), ]

    # Sort the group by the stat_overall column in ascending order
    if ("stat_overall" %in% names(group_rows)) {
      group_rows <- group_rows[order(group_rows$stat_overall, na.last = TRUE), ]
    }

    new_df <- rbind(new_df, group_rows)

    # Insert an empty row if this is not the last group
    if (i < length(r$lengths)) {
      # Create an empty row with NA values
      empty_row <- setNames(as.list(rep(NA, ncol(df))), names(df))

      # Copy values from the first and second columns of the last row
      empty_row[[1]] <- group_rows[nrow(group_rows), 1]
      empty_row[[2]] <- group_rows[nrow(group_rows), 2]

      new_df <- rbind(new_df, empty_row)
    }
    idx <- idx + group_length
  }

  return(new_df)
}

#' Format XLSX Variable by Group
#'
#' This function formats an existing data table and writes it to an Excel workbook with a
#' variable-by-group layout, applying specific styles such as percentage formatting,
#' background colors, and conditional color scales. It references all styles and row-insertion
#' steps internally, producing a complete formatted file in one call.
#'
#' @param table_group_x_variable A data frame or list containing the data to be formatted.
#' @param file_path Character string for the output Excel file path. Defaults to NULL (returns workbook).
#' @param table_name Character string indicating the name of the table if the input is a list.
#' @param value_columns Character vector matching value column headers.
#' @param total_columns Character vector matching total column headers.
#' @param readme_sheet_name Name of the readme sheet to be created.
#' @param table_sheet_name Name of the tabular data sheet to be created.
#' @param overwrite Logical indicating whether to overwrite existing Excel files.
#' @param insert_empty_rows Logical. If TRUE, empty rows will be inserted based on the `empty_rows_col`.
#' @param empty_rows_col Character string. The column name used for grouping when inserting empty rows.
#'
#' @return An `openxlsx` workbook object if `file_path` is NULL. Otherwise, it writes the workbook to `file_path`.
#' @export
#' @importFrom openxlsx createWorkbook addWorksheet writeData createStyle addStyle mergeCells freezePane conditionalFormatting saveWorkbook
#' @importFrom stringr str_starts str_c str_detect
format_my_xlsx_variable_x_group <- function(
  table_group_x_variable,
  file_path = NULL,
  table_name = "variable_x_group_table",
  value_columns = c("stat", "stat_low", "stat_upp"),
  total_columns = NULL,
  readme_sheet_name = "readme",
  table_sheet_name = "variable_x_group_table",
  overwrite = FALSE,
  insert_empty_rows = FALSE,
  empty_rows_col = "analysis_var"
) {
  # Check if it is a list or dataset.
  if (is.data.frame(table_group_x_variable)) {
    results_table_group_x_variable <- table_group_x_variable
  } else {
    # check if list contains all the elements.
    if (!all(c(table_name) %in% names(table_group_x_variable))) {
      stop(sprintf("Cannot identify '%s' element of the list.", table_name))
    }
    results_table_group_x_variable <- table_group_x_variable[[table_name]]
  }

  # Strip out UUID column immediately if it exists so it doesn't pollute the output
  if ("uuid" %in% names(results_table_group_x_variable)) {
    results_table_group_x_variable$uuid <- NULL
  }

  # Rename analysis_var -> question, analysis_var_value -> option for cleaner output
  if ("analysis_var" %in% names(results_table_group_x_variable)) {
    names(results_table_group_x_variable)[
      names(results_table_group_x_variable) == "analysis_var"
    ] <- "question"
  }
  if ("analysis_var_value" %in% names(results_table_group_x_variable)) {
    names(results_table_group_x_variable)[
      names(results_table_group_x_variable) == "analysis_var_value"
    ] <- "option"
  }
  # Resolve the empty_rows_col name after potential rename
  if (
    empty_rows_col == "analysis_var" &&
      "question" %in% names(results_table_group_x_variable)
  ) {
    empty_rows_col <- "question"
  }

  # Insert empty rows if requested
  if (
    isTRUE(insert_empty_rows) &&
      empty_rows_col %in% names(results_table_group_x_variable)
  ) {
    results_table_group_x_variable <- add_empty_rows_between_groups(
      results_table_group_x_variable,
      empty_rows_col
    )
  }

  # --- Setup Styles Internally so this function references everything it needs ---
  number_2digits_style <- openxlsx::createStyle(numFmt = "0.00")
  number_style <- openxlsx::createStyle(numFmt = "0")
  proportion_number_style <- openxlsx::createStyle(numFmt = "PERCENTAGE")

  thick_left_borderstyle <- openxlsx::createStyle(
    borderStyle = "thick",
    border = "left"
  )
  thick_top_borderstyle <- openxlsx::createStyle(
    borderStyle = "thick",
    border = "top"
  )
  thick_right_borderstyle <- openxlsx::createStyle(
    borderStyle = "thick",
    border = "right"
  )
  thick_bottom_borderstyle <- openxlsx::createStyle(
    borderStyle = "thick",
    border = "bottom"
  )

  header_style <- openxlsx::createStyle(
    fontSize = 12,
    fontColour = "#FFFFFF",
    halign = "center",
    valign = "center",
    fontName = "Arial Narrow",
    textDecoration = "bold",
    fgFill = "#EE5859",
    border = "TopBottomLeftRight",
    borderColour = "#fafafa",
    wrapText = FALSE
  )

  secondary_grey_cell_style <- openxlsx::createStyle(
    fontSize = 12,
    fontColour = "#58585A",
    fontName = "Arial Narrow",
    fgFill = "#c7c8ca",
    border = "TopBottomLeftRight ",
    wrapText = FALSE,
    halign = "center",
    valign = "center"
  )

  blank_row_style <- openxlsx::createStyle(
    fontSize = 12,
    fontColour = "#58585A",
    fontName = "Arial Narrow",
    fgFill = "#f5f5f5",
    wrapText = FALSE
  )

  secondary_beige_cell_style <- openxlsx::createStyle(
    fontSize = 12,
    fontColour = "#58585A",
    fontName = "Arial Narrow",
    fgFill = "#eae2b7",
    border = "TopBottomLeftRight",
    wrapText = FALSE
  )

  secondary_white_cell_style <- openxlsx::createStyle(
    fontSize = 12,
    fontColour = "#58585A",
    fontName = "Arial Narrow",
    fgFill = NULL,
    border = "TopBottomLeftRight",
    wrapText = FALSE,
    halign = "center",
    valign = "center"
  )

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = readme_sheet_name)
  openxlsx::addWorksheet(wb, sheetName = table_sheet_name)

  # write the data
  openxlsx::writeData(
    wb,
    sheet = table_sheet_name,
    x = results_table_group_x_variable,
    startRow = 2 # Write headers to row 2, data to row 3+
  )

  # count number of columns before "stat_"
  stat_match <- which(startsWith(
    names(results_table_group_x_variable),
    "stat_"
  ))
  if (length(stat_match) == 0) {
    stop("No columns starting with 'stat_' were found in the dataset.")
  }
  num_cols_before_stat <- stat_match[1] - 1

  col_names <- names(results_table_group_x_variable)
  num_cols <- length(col_names)

  #----------------------------------------------------------------------
  # 4. Loop through columns in sets of 3:
  #    - Merge them in row 1
  #    - Extract the group name from the first column (after "stat_")
  #    - Write that group name into the merged cell
  #    - Remove the "_<group>" substring from each of the headers
  #----------------------------------------------------------------------

  stat_length <- length(c(value_columns, total_columns))

  col_index <- 1
  while (col_index <= num_cols) {
    this_col_name <- col_names[col_index]

    if (startsWith(this_col_name, "stat_")) {
      group_name <- sub("^stat_", "", this_col_name) # e.g. "stat_overall" -> "overall"
      # If the DAP group_var was NA (Overall group), label it "Overall" instead of "NA"
      if (group_name == "NA") {
        group_name <- "Overall"
      }
      merge_start <- col_index
      merge_end <- col_index + (stat_length - 1)

      if (merge_end > num_cols) {
        stop(
          "Not enough columns to merge based on sets indicated by value_columns/total_columns count. Check your data."
        )
      }

      openxlsx::mergeCells(
        wb,
        sheet = table_sheet_name,
        rows = 1,
        cols = merge_start:merge_end
      )

      openxlsx::writeData(
        wb,
        sheet = table_sheet_name,
        x = group_name,
        startRow = 1,
        startCol = merge_start,
        colNames = FALSE,
        rowNames = FALSE
      )

      for (c in merge_start:merge_end) {
        col_names[c] <- sub(paste0("_", group_name), "", col_names[c])
      }

      col_index <- col_index + stat_length
    } else {
      col_index <- col_index + 1
    }
  }

  #----------------------------------------------------------------------
  # 5. Update the headers in row 2 with the new col_names
  #----------------------------------------------------------------------
  openxlsx::writeData(
    wb,
    sheet = table_sheet_name,
    x = t(col_names),
    startRow = 2,
    startCol = 1,
    colNames = FALSE,
    rowNames = FALSE
  )

  # Formatting columns
  variable_min_cols_index <- 1
  variable_max_cols_index <- num_cols_before_stat

  # Identify rows where 'question' is not NA (if column exists)
  if ("question" %in% names(results_table_group_x_variable)) {
    non_na_question_rows <- which(
      !is.na(results_table_group_x_variable$question)
    )
  } else {
    non_na_question_rows <- 1:nrow(results_table_group_x_variable)
  }

  # Apply styles dynamically referencing the exact row lengths
  max_row_index <- nrow(results_table_group_x_variable) + 2

  openxlsx::addStyle(
    wb,
    sheet = table_sheet_name,
    style = secondary_beige_cell_style,
    rows = 2:max_row_index,
    cols = c(variable_min_cols_index:variable_max_cols_index),
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    wb,
    sheet = table_sheet_name,
    style = thick_left_borderstyle,
    rows = 2:max_row_index,
    cols = c(variable_min_cols_index, variable_min_cols_index),
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    wb,
    sheet = table_sheet_name,
    style = thick_top_borderstyle,
    rows = 2,
    cols = c(variable_min_cols_index:variable_max_cols_index),
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    wb,
    sheet = table_sheet_name,
    style = thick_right_borderstyle,
    rows = c(2, non_na_question_rows + 2),
    cols = c(variable_max_cols_index:variable_max_cols_index),
    gridExpand = TRUE,
    stack = TRUE
  )

  openxlsx::addStyle(
    wb,
    sheet = table_sheet_name,
    style = thick_bottom_borderstyle,
    rows = max_row_index,
    cols = c(variable_min_cols_index:variable_max_cols_index),
    gridExpand = TRUE,
    stack = TRUE
  )

  ## formatting stats columns
  stat_min_cols_index <- variable_max_cols_index + 1
  stat_max_cols_index <- ncol(results_table_group_x_variable)

  stat_total_cols <- stat_max_cols_index - variable_max_cols_index
  stat_sets_number <- stat_total_cols / stat_length

  if ((stat_total_cols %% stat_length) != 0) {
    stop(
      "Length of value_columns/total_columns does not align fully with the table."
    )
  }

  helper_table <- data.frame(
    sets = paste0("set", 1:stat_sets_number),
    min_cols_index = cumsum(c(
      stat_min_cols_index,
      rep(stat_length, stat_sets_number - 1)
    )),
    max_cols_index = cumsum(c(
      stat_min_cols_index + stat_length - 1,
      rep(stat_length, stat_sets_number - 1)
    ))
  )

  if (stat_length == 1) {
    warning(
      "Length of value_columns/total_columns is one, function cannot strictly check the number of columns."
    )
  }

  ### formatting the numbers
  value_columns_index <- stringr::str_starts(
    names(results_table_group_x_variable),
    stringr::str_c(value_columns, collapse = "|")
  )

  if ("analysis_type" %in% names(results_table_group_x_variable)) {
    proportion_rows <- which(grepl(
      "prop_select",
      results_table_group_x_variable$analysis_type
    )) +
      2
    non_proportion_rows <- which(
      !grepl("prop_select", results_table_group_x_variable$analysis_type)
    ) +
      2
  } else {
    proportion_rows <- integer(0)
    non_proportion_rows <- 2:max_row_index
  }

  if (length(proportion_rows) > 0 && sum(value_columns_index) > 0) {
    openxlsx::addStyle(
      wb,
      sheet = table_sheet_name,
      style = proportion_number_style,
      rows = proportion_rows,
      cols = which(value_columns_index),
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  if (length(non_proportion_rows) > 0 && sum(value_columns_index) > 0) {
    openxlsx::addStyle(
      wb,
      sheet = table_sheet_name,
      style = number_2digits_style,
      rows = non_proportion_rows,
      cols = which(value_columns_index),
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  if (!is.null(total_columns)) {
    total_columns_index <- stringr::str_starts(
      names(results_table_group_x_variable),
      stringr::str_c(total_columns, collapse = "|")
    )
    if (sum(total_columns_index) > 0) {
      openxlsx::addStyle(
        wb,
        sheet = table_sheet_name,
        style = number_style,
        rows = 2:max_row_index,
        cols = which(total_columns_index),
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }

  ### formatting the background
  for (i in 1:nrow(helper_table)) {
    if ((i %% 2) != 0) {
      openxlsx::addStyle(
        wb,
        sheet = table_sheet_name,
        style = secondary_white_cell_style,
        rows = 2:max_row_index,
        cols = c(
          helper_table[i, "min_cols_index"]:helper_table[i, "max_cols_index"]
        ),
        gridExpand = TRUE,
        stack = TRUE
      )
    } else {
      openxlsx::addStyle(
        wb,
        sheet = table_sheet_name,
        style = secondary_grey_cell_style,
        rows = 2:max_row_index,
        cols = c(
          helper_table[i, "min_cols_index"]:helper_table[i, "max_cols_index"]
        ),
        gridExpand = TRUE,
        stack = TRUE
      )
    }

    openxlsx::addStyle(
      wb,
      sheet = table_sheet_name,
      style = thick_left_borderstyle,
      rows = 2:max_row_index,
      cols = c(
        helper_table[i, "min_cols_index"]:helper_table[i, "min_cols_index"]
      ),
      gridExpand = TRUE,
      stack = TRUE
    )
    openxlsx::addStyle(
      wb,
      sheet = table_sheet_name,
      style = thick_top_borderstyle,
      rows = 2,
      cols = c(
        helper_table[i, "min_cols_index"]:helper_table[i, "max_cols_index"]
      ),
      gridExpand = TRUE,
      stack = TRUE
    )
    openxlsx::addStyle(
      wb,
      sheet = table_sheet_name,
      style = thick_right_borderstyle,
      rows = 2:max_row_index,
      cols = c(
        helper_table[i, "max_cols_index"]:helper_table[i, "max_cols_index"]
      ),
      gridExpand = TRUE,
      stack = TRUE
    )
    openxlsx::addStyle(
      wb,
      sheet = table_sheet_name,
      style = thick_bottom_borderstyle,
      rows = max_row_index,
      cols = c(
        helper_table[i, "min_cols_index"]:helper_table[i, "max_cols_index"]
      ),
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  # Add grey background style to empty rows
  if (ncol(results_table_group_x_variable) >= 3) {
    na_rows <- which(is.na(results_table_group_x_variable[[3]]))
    if (length(na_rows) > 0) {
      openxlsx::addStyle(
        wb,
        sheet = table_sheet_name,
        style = blank_row_style,
        rows = na_rows + 2,
        cols = 1:ncol(results_table_group_x_variable),
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }

  # Conditional formatting: target the first stat_ column after the 'option' column
  # (previously 'analysis_var_value' before rename)
  option_col_index <- which(
    names(results_table_group_x_variable) %in% c("option", "analysis_var_value")
  )
  if (length(option_col_index) > 0) {
    # Find the first column starting with stat_ that comes after the option column
    first_stat_col_index <- which(startsWith(
      names(results_table_group_x_variable),
      "stat_"
    ))
    first_stat_col_index <- first_stat_col_index[
      first_stat_col_index > option_col_index[1]
    ][1]
  } else {
    first_stat_col_index <- which(startsWith(
      names(results_table_group_x_variable),
      "stat_"
    ))[1]
  }

  if (!is.na(first_stat_col_index)) {
    # Use the first stat_ column to detect per-question groups via run-length encoding
    stat_first_col_vals <- results_table_group_x_variable[[
      first_stat_col_index
    ]]
    r2 <- rle(!is.na(stat_first_col_vals))
    start_idx2 <- cumsum(c(1, r2$lengths[-length(r2$lengths)]))
    end_idx2 <- start_idx2 + r2$lengths - 1
    valid_groups2 <- which(r2$lengths > 1 & r2$values == TRUE)

    for (group in valid_groups2) {
      rows <- start_idx2[group]:end_idx2[group]
      openxlsx::conditionalFormatting(
        wb,
        sheet = table_sheet_name,
        cols = first_stat_col_index,
        rows = rows + 2,
        type = "colorScale",
        style = c("#fff0f3", "#f9b4b3", "#EE5859")
      )
    }
  }

  # Finding where to freeze panes dynamically — after renaming, look for "option" first, then fall back
  analysis_var_col_index <- which(
    names(results_table_group_x_variable) == "option"
  )
  if (length(analysis_var_col_index) == 0) {
    analysis_var_col_index <- which(
      names(results_table_group_x_variable) == "analysis_var_value"
    )
  }
  if (length(analysis_var_col_index) > 0) {
    freeze_col <- analysis_var_col_index[1] + 1
  } else {
    freeze_col <- 5
  }

  openxlsx::addStyle(
    wb,
    sheet = table_sheet_name,
    style = header_style,
    rows = 1,
    cols = freeze_col:ncol(results_table_group_x_variable),
    gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb,
    sheet = table_sheet_name,
    style = header_style,
    rows = 2,
    cols = 1:ncol(results_table_group_x_variable),
    gridExpand = TRUE
  )

  openxlsx::freezePane(
    wb,
    table_sheet_name,
    firstActiveRow = 3,
    firstActiveCol = freeze_col
  )

  if (is.null(file_path)) {
    return(wb)
  } else {
    if (stringr::str_detect(file_path, "\\.xlsx", negate = TRUE)) {
      stop("file_path does not contain .xlsx")
    }
    openxlsx::saveWorkbook(wb, file_path, overwrite = overwrite)
  }
}
