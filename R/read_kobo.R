#' Read Kobo Survey Sheet
#'
#' Reads and processes the survey sheet from a Kobo XLS form. Column names are
#' lowercased, rows without a name are removed, and \code{q_type} and
#' \code{list_name} columns are extracted from the \code{type} column.
#'
#' @param filepath Path to the Kobo XLS/XLSX tool file.
#' @param sheet_name Name of the survey sheet (default is "survey").
#' @return A dataframe containing the processed survey sheet with added
#'   \code{q_type} and \code{list_name} columns.
#' @export
read_kobo_survey <- function(filepath, sheet_name = "survey") {
  kobo_survey <- readxl::read_excel(
    filepath,
    sheet = sheet_name,
    col_types = "text"
  ) %>%
    rename_with(tolower) %>%
    filter(!is.na(name)) %>%
    mutate(
      q_type = as.character(lapply(type, function(x) {
        str_split(x, " ")[[1]][1]
      })),
      list_name = as.character(lapply(type, function(x) {
        str_split(x, " ")[[1]][2]
      }))
    )

  cat(crayon::green(paste0(
    "--> Kobo survey loaded: ",
    nrow(kobo_survey), " questions\n"
  )))

  return(kobo_survey)
}

#' Read Kobo Choices Sheet
#'
#' Reads and processes the choices sheet from a Kobo XLS form. Column names
#' are lowercased, rows without a \code{list_name} are removed, and
#' duplicate entries are dropped.
#'
#' @param filepath Path to the Kobo XLS/XLSX tool file.
#' @param sheet_name Name of the choices sheet (default is "choices").
#' @return A dataframe containing the processed choices sheet with columns
#'   \code{list_name}, \code{name}, and \code{label}.
#' @export
read_kobo_choices <- function(filepath, sheet_name = "choices") {
  kobo_choices <- readxl::read_excel(
    filepath,
    sheet = sheet_name,
    col_types = "text"
  ) %>%
    rename_with(tolower) %>%
    filter(!is.na(list_name)) %>%
    select(list_name, name, label) %>%
    distinct()

  cat(crayon::green(paste0(
    "--> Kobo choices loaded: ",
    nrow(kobo_choices), " choices\n"
  )))

  return(kobo_choices)
}
