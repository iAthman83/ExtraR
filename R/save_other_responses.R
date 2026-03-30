#' Prepare Other Responses Dataframe
#'
#' Combines other responses from all sheets/loops into a single dataframe
#' ready for use with \code{save_other_responses()}. The function dynamically
#' includes extra columns (e.g., enumerator, sample_area) if provided.
#'
#' @param raw_data Main raw dataset dataframe (always required).
#' @param other_db Dataframe from \code{get_other_db()} with question metadata.
#' @param kobo_choices Dataframe of Kobo choices sheet.
#' @param raw_loops Optional list of loop/roster dataframes (default is NULL).
#'   Pass as a named or unnamed list, e.g.,
#'   \code{list(household_roster = df1, children = df2)} or \code{list(df1, df2)}.
#' @param extra_columns Optional character vector of additional column names
#'   to include from the raw data (e.g., \code{c("enumerator", "sample_area")}).
#'   Default is NULL.
#' @param uuid_column Name of the uuid column in raw_data (default is "_uuid").
#' @return A dataframe formatted for \code{save_other_responses()}.
#' @export
prepare_other_responses <- function(
  raw_data,
  other_db,
  kobo_choices,
  raw_loops = NULL,
  extra_columns = NULL,
  uuid_column = "_uuid"
) {
  # Rename uuid column if needed
  if (uuid_column != "uuid" && uuid_column %in% colnames(raw_data)) {
    raw_data <- raw_data %>% rename(uuid = !!sym(uuid_column))
  }

  # Rename uuid column in all loop datasets
  if (!is.null(raw_loops)) {
    raw_loops <- lapply(raw_loops, function(loop_df) {
      if (uuid_column != "uuid" && uuid_column %in% colnames(loop_df)) {
        loop_df <- loop_df %>% rename(uuid = !!sym(uuid_column))
      }
      return(loop_df)
    })
  }

  # --- Main dataset other responses ---
  var_other_raw <- other_db$name[other_db$name %in% colnames(raw_data)]

  select_cols_main <- c("uuid", var_other_raw)
  if (!is.null(extra_columns)) {
    select_cols_main <- c(select_cols_main, intersect(extra_columns, colnames(raw_data)))
  }

  other_responses_main <- raw_data %>%
    select(all_of(select_cols_main)) %>%
    pivot_longer(
      cols = all_of(var_other_raw),
      names_to = "question_name",
      values_to = "response_en"
    ) %>%
    filter(!is.na(response_en))

  # --- Loop datasets other responses (if provided) ---
  other_responses <- other_responses_main

  if (!is.null(raw_loops) && length(raw_loops) > 0) {
    for (loop_df in raw_loops) {
      var_other_loop <- other_db$name[other_db$name %in% colnames(loop_df)]

      if (length(var_other_loop) > 0) {
        select_cols_loop <- c("uuid", var_other_loop)
        if (!is.null(extra_columns)) {
          select_cols_loop <- c(select_cols_loop, intersect(extra_columns, colnames(loop_df)))
        }

        other_responses_loop <- loop_df %>%
          select(all_of(select_cols_loop)) %>%
          pivot_longer(
            cols = all_of(var_other_loop),
            names_to = "question_name",
            values_to = "response_en"
          ) %>%
          filter(!is.na(response_en))

        other_responses <- bind_rows(other_responses, other_responses_loop)
      }
    }
  }

  # Build the dynamic select columns
  select_cols <- c("uuid")
  if (!is.null(extra_columns)) {
    select_cols <- c(select_cols, intersect(extra_columns, colnames(other_responses)))
  }
  select_cols <- c(select_cols, "question_name", "list_name", "full_label", "response_en")

  # Generate final dataframe
  df <- other_responses %>%
    arrange(question_name, uuid) %>%
    left_join(
      select(other_db, name, full_label, list_name),
      by = c("question_name" = "name")
    ) %>%
    select(all_of(select_cols)) %>%
    mutate(
      "TRUE other (copy response_en or provide a better translation)" = NA,
      "EXISTING other 1 (select the most appropriate choice)" = NA,
      "EXISTING other 2 (select the most appropriate choice)" = NA,
      "EXISTING other 3 (select the most appropriate choice)" = NA,
      "INVALID other (select yes or leave blank)" = NA,
      "FOLLOW-UP message (what is unclear about this response?)" = NA,
      "Explanation" = NA,
      selected_choices = NA
    )

  # Add selected choices for select_multiple questions
  for (r in 1:nrow(df)) {
    ref_name <- other_db[["ref_question"]][other_db$name == df$question_name[r]]
    q_type <- other_db[["q_type"]][other_db$name == df$question_name[r]]
    if (length(q_type) > 0 && q_type %in% "select_multiple") {
      # Search main data first, then loop datasets
      value <- raw_data[[ref_name]][raw_data$uuid == df$uuid[r]]
      if ((length(value) == 0 || is.na(value)) && !is.null(raw_loops)) {
        for (loop_df in raw_loops) {
          if (ref_name %in% colnames(loop_df) && "uuid" %in% colnames(loop_df)) {
            value <- loop_df[[ref_name]][loop_df$uuid == df$uuid[r]]
            if (length(value) > 0 && !is.na(value)) break
          }
        }
      }
      if (length(value) > 0 && !is.na(value)) {
        choices <- str_split(value, " ")[[1]]
        labels <- unlist(lapply(choices, function(c) {
          get_label_from_name(df$list_name[r], c, kobo_choices)
        }))
        df$selected_choices[r] <- paste0(labels, collapse = ";\n")
      }
    }
  }

  return(df)
}

#' Save Other Responses
#'
#' This function saves the other responses into an Excel workbook with specific formatting and data validation.
#' The target columns are identified dynamically by name, allowing flexibility such as adding an `enumerator_id`.
#'
#' @param df Data frame containing the responses to write.
#' @param ref_date Reference date for the filename (default is "").
#' @param enumerator_id Optional string indicating the enumerator id column (default is NULL).
#' @param save_location Directory to save the output file (default is "output").
#' @param other_db Data frame containing dropdown mapping for existing choices (default is NULL).
#' @return NULL. Saves an Excel file.
#' @export
save_other_responses <- function(
  df,
  ref_date = "",
  enumerator_id = NULL,
  save_location = "output",
  other_db = NULL
) {
  get_column_letter <- function(r) {
    return(ifelse(
      r <= 26,
      LETTERS[r],
      ifelse(
        r <= 52,
        paste0(LETTERS[1], LETTERS[r - 26]),
        paste0(LETTERS[2], LETTERS[r - 52])
      )
    ))
  }

  # save other responses
  wb <- createWorkbook()
  style.col.color <- createStyle(
    fgFill = "#E5FFCC",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    valign = "top",
    wrapText = T
  )
  style.col.color1 <- createStyle(
    fgFill = "#E5FFEC",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    valign = "top",
    wrapText = T
  )
  style.col.color2 <- createStyle(
    fgFill = "#CCE5FF",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    valign = "top",
    wrapText = T
  )
  style.col.color.first <- createStyle(
    textDecoration = "bold",
    fgFill = "#E5FFCC",
    valign = "top",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    wrapText = T
  )
  style.col.color.first1 <- createStyle(
    textDecoration = "bold",
    fgFill = "#E5FFEC",
    valign = "top",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    wrapText = T
  )
  style.col.color.first2 <- createStyle(
    textDecoration = "bold",
    fgFill = "#CCE5FF",
    valign = "top",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    wrapText = T
  )

  addWorksheet(wb, "Sheet1")
  writeData(wb = wb, x = df, sheet = "Sheet1", startRow = 1)

  exist_cols <- which(
    names(df) %in%
      c(
        "EXISTING other 1 (select the most appropriate choice)",
        "EXISTING other 2 (select the most appropriate choice)",
        "EXISTING other 3 (select the most appropriate choice)"
      )
  )
  invalid_col <- which(names(df) == "INVALID other (select yes or leave blank)")

  # Dynamic styling based on column positions and names
  for (i in seq_along(names(df))) {
    if (i %in% exist_cols) {
      addStyle(
        wb,
        "Sheet1",
        style = style.col.color1,
        rows = 1:(nrow(df) + 1),
        cols = i
      )
      addStyle(wb, "Sheet1", style = style.col.color.first1, rows = 1, cols = i)
    } else if (i %in% invalid_col) {
      addStyle(
        wb,
        "Sheet1",
        style = style.col.color,
        rows = 1:(nrow(df) + 1),
        cols = i
      )
      addStyle(wb, "Sheet1", style = style.col.color.first, rows = 1, cols = i)
    } else if (length(exist_cols) > 0 && i == min(exist_cols) - 1) {
      addStyle(
        wb,
        "Sheet1",
        style = style.col.color,
        rows = 1:(nrow(df) + 1),
        cols = i
      )
      addStyle(wb, "Sheet1", style = style.col.color.first, rows = 1, cols = i)
    } else if (length(invalid_col) > 0 && i > max(invalid_col)) {
      addStyle(
        wb,
        "Sheet1",
        style = style.col.color2,
        rows = 1:(nrow(df) + 1),
        cols = i
      )
      addStyle(wb, "Sheet1", style = style.col.color.first2, rows = 1, cols = i)
    } else {
      addStyle(
        wb,
        "Sheet1",
        style = createStyle(valign = "top", wrapText = T),
        rows = 1:(nrow(df) + 1),
        cols = i
      )
      addStyle(
        wb,
        "Sheet1",
        style = createStyle(textDecoration = "bold"),
        rows = 1,
        cols = i
      )
    }
  }

  addWorksheet(wb, "Dropdown_values")
  if (!is.null(other_db) && nrow(other_db) > 0) {
    for (r in 1:nrow(other_db)) {
      if (other_db$q_type[r] != "text") {
        choices <- str_split(other_db$choices[r], ";;")[[1]]
        writeData(wb, sheet = "Dropdown_values", x = choices, startCol = r)
        uuids <- which(df$question_name == other_db$name[r])
        if (length(uuids) > 0 && length(exist_cols) > 0) {
          column_letter <- get_column_letter(r)
          values <- paste0(
            "'Dropdown_values'!$",
            column_letter,
            "$1:$",
            column_letter,
            "$",
            other_db$num_choices[r]
          )
          for (c_idx in exist_cols) {
            dataValidation(
              wb,
              "Sheet1",
              col = c_idx,
              rows = uuids + 1,
              type = "list",
              value = values
            )
          }
        }
      }
    }
  } else {
    r <- 0
  }

  writeData(wb, sheet = "Dropdown_values", x = c("Yes"), startCol = r + 1)
  column_letter <- get_column_letter(r + 1)
  values <- paste0(
    "'Dropdown_values'!$",
    column_letter,
    "$1:$",
    column_letter,
    "$1"
  )
  if (length(invalid_col) > 0) {
    dataValidation(
      wb,
      "Sheet1",
      col = invalid_col,
      rows = 2:(nrow(df) + 1),
      type = "list",
      value = values
    )
  }

  setColWidths(wb, "Sheet1", cols = 1:ncol(df), widths = 25)
  setColWidths(wb, "Sheet1", cols = 1:min(5, ncol(df)), widths = 15)

  modifyBaseFont(wb, fontSize = 12, fontColour = "black", fontName = "Calibri")

  sub.filename <- paste0(Sys.Date(), "_other_responses.xlsx")
  if (!dir.exists(save_location)) {
    dir.create(save_location, recursive = TRUE)
  }

  saveWorkbook(
    wb,
    file.path(save_location, sub.filename),
    overwrite = T
  )
}
