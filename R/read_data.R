#' Read Raw Main Dataset
#'
#' Reads the main raw dataset from an Excel file, converts date and datetime
#' columns based on the Kobo survey definition, renames key identifier columns,
#' and converts integer/decimal columns to numeric.
#'
#' @param filename Path to the Excel file containing the raw data.
#' @param kobo_survey A dataframe containing the Kobo survey sheet (used for
#'   identifying date and numeric columns).
#' @param extra_date_cols Optional character vector of additional date column
#'   names to convert (default is NULL). These are appended to the auto-detected
#'   "today" type columns from the survey.
#' @param extra_datetime_cols Optional character vector of additional datetime
#'   column names to convert (default is NULL). These are appended to the
#'   standard \code{c("start_time", "end_time", "_submission_time")}.
#' @param sheet Sheet number or name to read from the Excel file (default is 1).
#' @return A dataframe with dates converted, identifiers renamed, and numeric
#'   columns cast to numeric.
#' @export
read_raw_data <- function(
  filename,
  kobo_survey,
  extra_date_cols = NULL,
  extra_datetime_cols = NULL,
  sheet = 1
) {
  df <- readxl::read_excel(filename, sheet = sheet, col_types = "text")
  cat(crayon::green(paste0(
    "--> RAW --> ",
    filename,
    " --> ",
    nrow(df),
    " entries, ",
    ncol(df),
    " columns \n"
  )))

  # --- Auto-detect date and datetime columns from kobo_survey ---
  cat(crayon::green("--> convert dates \n"))
  # Date columns: survey types "today" and "date"
  cols_date <- c(
    kobo_survey$name[kobo_survey$type %in% c("today", "date")],
    extra_date_cols
  )
  cols_date <- intersect(cols_date, colnames(df))

  # Create submission_date from _submission_time if it exists
  if ("_submission_time" %in% colnames(df)) {
    df <- df %>% mutate(submission_date = `_submission_time`)
    cols_date <- unique(c(cols_date, "submission_date"))
  }

  if (length(cols_date) > 0) {
    df <- df %>%
      mutate_at(
        cols_date,
        ~ ifelse(
          is.na(.),
          NA,
          as.character(as.Date(openxlsx::convertToDate(as.numeric(.))))
        )
      )
  }

  # --- Convert datetime columns ---
  # Datetime columns: survey types "start", "end", "datetime"
  cols_datetime <- c(
    kobo_survey$name[kobo_survey$type %in% c("start", "end", "datetime")],
    "_submission_time",
    extra_datetime_cols
  )
  cols_datetime <- intersect(cols_datetime, colnames(df))

  if (length(cols_datetime) > 0) {
    df <- df %>%
      mutate_at(
        cols_datetime,
        ~ ifelse(
          is.na(.),
          NA,
          as.character(openxlsx::convertToDateTime(as.numeric(.)))
        )
      )
  }

  # --- Rename id/uuid columns ---
  cat(crayon::green("--> rename id/uuid and add submission time \n"))
  if ("_id" %in% colnames(df)) {
    df <- df %>% rename(id = "_id")
  }
  if ("_uuid" %in% colnames(df)) {
    df <- df %>% rename(uuid = "_uuid")
  }
  if ("_submission_time" %in% colnames(df)) {
    df <- df %>% rename(submission_time = "_submission_time")
  }
  df <- df %>% mutate(df_name = "main")

  # --- Convert numeric columns ---
  cat(crayon::green("--> convert integer columns to numeric \n"))
  cols_numeric <- get_cols_numeric(kobo_survey)
  df <- df %>%
    mutate_at(intersect(cols_numeric, colnames(df)), as.numeric)
  names(df) <- gsub("/", ".", names(df))

  return(df)
}

#' Read Loop/Roster Dataset
#'
#' Reads a loop or roster dataset from an Excel file, converts date and datetime
#' columns, generates a composite UUID (row number + parent UUID), and converts
#' integer/decimal columns to numeric.
#'
#' @param filename Path to the Excel file containing the loop data.
#' @param kobo_survey A dataframe containing the Kobo survey sheet (used for
#'   identifying numeric columns).
#' @param sheet Sheet number or name to read from the Excel file.
#' @param loop_name A string label for this loop (e.g., "household_roster",
#'   "children"). This is stored in a \code{df_name} column.
#' @param uuid_column Name of the parent UUID column in the loop data
#'   (default is "_submission__uuid").
#' @param submission_time_column Name of the submission time column in the loop
#'   data (default is "_submission__submission_time").
#' @return A dataframe with a composite UUID, dates converted, and numeric
#'   columns cast to numeric.
#' @export
read_loop_data <- function(
  filename,
  kobo_survey,
  sheet,
  loop_name,
  uuid_column = "_submission__uuid",
  submission_time_column = "_submission__submission_time"
) {
  df <- readxl::read_excel(filename, sheet = sheet, col_types = "text")
  cat(crayon::green(paste0(
    "--> RAW LOOP [",
    loop_name,
    "] --> ",
    filename,
    " --> ",
    nrow(df),
    " entries, ",
    ncol(df),
    " columns \n"
  )))

  # --- Auto-detect date and datetime columns from kobo_survey ---
  cat(crayon::green("--> convert dates \n"))
  # Date columns: survey types "today" and "date"
  cols_date <- intersect(
    kobo_survey$name[kobo_survey$type %in% c("today", "date")],
    colnames(df)
  )

  # Create submission_date from submission time column if it exists
  if (submission_time_column %in% colnames(df)) {
    df <- df %>% mutate(submission_date = !!sym(submission_time_column))
    cols_date <- unique(c(cols_date, "submission_date"))
  }

  if (length(cols_date) > 0) {
    df <- df %>%
      mutate_at(
        cols_date,
        ~ ifelse(
          is.na(.),
          NA,
          as.character(as.Date(openxlsx::convertToDate(as.numeric(.))))
        )
      )
  }

  # Datetime columns: survey types "start", "end", "datetime"
  cols_datetime <- c(
    kobo_survey$name[kobo_survey$type %in% c("start", "end", "datetime")],
    submission_time_column
  )
  cols_datetime <- intersect(cols_datetime, colnames(df))

  if (length(cols_datetime) > 0) {
    df <- df %>%
      mutate_at(
        cols_datetime,
        ~ ifelse(
          is.na(.),
          NA,
          as.character(openxlsx::convertToDateTime(as.numeric(.)))
        )
      )
  }

  # --- Generate composite UUID (row_number within each parent uuid + parent uuid) ---
  cat(crayon::green("--> generate composite uuid \n"))
  if (uuid_column %in% colnames(df)) {
    df <- df %>%
      group_by(!!sym(uuid_column)) %>%
      mutate(
        .loop_index = row_number(),
        uuid = paste0(.loop_index, "_", !!sym(uuid_column))
      ) %>%
      ungroup() %>%
      select(-.loop_index)
  } else {
    warning(paste0(
      "UUID column '",
      uuid_column,
      "' not found in loop data. ",
      "Cannot generate composite UUID."
    ))
  }

  df <- df %>% mutate(df_name = loop_name)

  # --- Convert numeric columns ---
  cat(crayon::green("--> convert integer columns to numeric \n"))
  cols_numeric <- get_cols_numeric(kobo_survey)
  df <- df %>%
    mutate_at(intersect(cols_numeric, colnames(df)), as.numeric)
  names(df) <- gsub("/", ".", names(df))

  return(df)
}
