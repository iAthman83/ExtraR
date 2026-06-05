# Define global variables to suppress R CMD check warnings about
# "no visible binding for global variable" (commonly found when using dplyr / tidyverse)
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".data",
    ".loop_index",
    "_submission_time",
    "analysis_type",
    "analysis_var",
    "analysis_var_value",
    "cleaning_log_other",
    "full_label",
    "group_var",
    "group_var_value",
    "label",
    "list_name",
    "name",
    "q_type",
    "question_name",
    "ref_question",
    "relevant",
    "response_en",
    "type",
    "uuid"
  ))
}

#' @import dplyr
#' @import stringr
#' @import openxlsx
#' @import tidyr
#' @import jsonlite
#' @import addindicators
#' @import pak
#' @importFrom rlang .data sym
#' @importFrom purrr map map_lgl map2
#' @importFrom crayon green yellow red
#' @importFrom stats setNames
#' @importFrom utils install.packages
NULL
