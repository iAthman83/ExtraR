#' Extract Referenced Question Name from Relevance Expression
#'
#' Parses a Kobo relevance expression string to extract the referenced
#' question name enclosed in curly braces (e.g., \code{${question_name}}).
#'
#' @param x A string containing a Kobo relevance expression.
#' @return The extracted question name, or \code{NA} if not found.
#' @keywords internal
get_ref_question <- function(x) {
  x.1 <- str_split(x, "\\{")[[1]][2]
  return(str_split(x.1, "\\}")[[1]][1])
}

#' Get Column Value by UUID
#'
#' Retrieves a value from a specific column for a given UUID. If the UUID
#' contains an underscore (indicating a loop entry), it searches the loop
#' dataset; otherwise it searches the main dataset.
#'
#' @param uuid The UUID to look up.
#' @param column The column name to retrieve the value from.
#' @param raw_data The main raw dataset dataframe.
#' @param raw_loop The loop/roster dataset dataframe.
#' @return The value from the specified column for the matching UUID.
#' @keywords internal
get_value_from_uuid <- function(uuid, column, raw_data, raw_loop) {
  if (str_detect(uuid, "_")) {
    value <- raw_loop[[column]][raw_loop$uuid == uuid]
  } else {
    value <- raw_data[[column]][raw_data$uuid == uuid]
  }
  return(value)
}

#' Get Choice Label from Choice Name
#'
#' Looks up the label for a given choice name within a specific list
#' in the Kobo choices sheet.
#'
#' @param list.name The \code{list_name} value to filter by.
#' @param name The choice \code{name} value to look up.
#' @param kobo_choices A dataframe containing the Kobo choices sheet.
#' @return The label string for the matching choice.
#' @keywords internal
get_label_from_name <- function(list.name, name, kobo_choices) {
  return(kobo_choices$label[
    kobo_choices$list_name == list.name & kobo_choices$name == name
  ])
}

#' Get Numeric Column Names from Kobo Survey
#'
#' Extracts the names of columns that are integer or decimal types
#' from the Kobo survey sheet.
#'
#' @param kobo_survey A dataframe containing the Kobo survey sheet.
#' @return A character vector of column names with numeric types.
get_cols_numeric <- function(kobo_survey) {
  kobo_survey %>%
    filter(type %in% c("integer", "decimal")) %>%
    pull(name)
}

