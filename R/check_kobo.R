#' Check if Answer Exists in Choices List
#'
#' Validates a single constraint expression of the form
#' \code{selected(${question_name}, 'answer')} by checking whether the
#' answer value exists in the corresponding choices list.
#'
#' @param questions A dataframe containing the Kobo survey sheet.
#' @param choices A dataframe containing the Kobo choices sheet.
#' @param constraint A string containing a single constraint expression.
#' @return \code{TRUE} if the answer is valid or the constraint is not
#'   a selected() type, \code{FALSE} otherwise.
#' @keywords internal
check_answer_in_list <- function(questions, choices, constraint) {
  if (!str_detect(constraint, ",")) return(TRUE)

  question_regex <- "\\{([^()]+)\\}"
  answer_regex <- "\\'([^()]+)\\'"

  question <- gsub(
    question_regex, "\\1",
    str_extract_all(constraint, question_regex)[[1]]
  )
  answer <- gsub(
    answer_regex, "\\1",
    str_extract_all(constraint, answer_regex)[[1]]
  )

  question_type <- questions %>%
    filter(name == question) %>%
    filter(!grepl("^(begin|end)\\s+group$", type)) %>%
    pull(type)

  if (length(question_type) == 0 || question_type == "calculate") {
    return(TRUE)
  } else {
    listname <- gsub("^.*\\s", "", question_type)
    choices_list <- choices %>%
      filter(list_name == listname) %>%
      pull(name)
    return(answer %in% choices_list)
  }
}

#' Check Kobo Survey Constraints
#'
#' Scans the relevant column of a Kobo survey sheet for all
#' \code{selected()} constraint expressions and validates that each
#' referenced answer exists in the choices sheet. Returns a character
#' vector of invalid constraints, if any.
#'
#' @param questions A dataframe containing the Kobo survey sheet.
#' @param choices A dataframe containing the Kobo choices sheet.
#' @return A character vector of invalid constraint expressions, or
#'   \code{NULL} if all constraints are valid.
#' @export
check_constraints <- function(questions, choices) {
  # Verify survey and choices using cleaningtools package
  if (!cleaningtools::verify_valid_survey(questions)) {
    stop("Survey sheet does not meet all the criteria. Please check!")
  }
  if (!cleaningtools::verify_valid_choices(choices)) {
    stop("Choices sheet does not meet all the criteria. Please check!")
  }

  questions <- mutate_at(questions, c("name", "type"), ~ str_trim(.))
  choices <- mutate_at(choices, c("list_name", "name"), ~ str_trim(.))

  all_constraints <- questions %>%
    filter(grepl("selected", relevant)) %>%
    pull(relevant)
  all_constraints <- gsub('"', "'", all_constraints)

  # Extract all selected() expressions and validate each
  selected_pattern <- "selected\\s*\\([^\\)]*\\)"

  rs_list <- purrr::map(
    all_constraints,
    ~ purrr::map_lgl(
      str_extract_all(.x, selected_pattern)[[1]],
      ~ check_answer_in_list(questions, choices, .x)
    )
  )

  invalid <- purrr::map2(
    rs_list, seq_along(rs_list),
    ~ if (length(which(!.x)) != 0) {
      return(
        str_extract_all(all_constraints[.y], selected_pattern)[[1]][which(!.x)]
      )
    }
  ) %>%
    unlist() %>%
    unique()

  if (length(invalid) == 0) {
    cat(crayon::green("All constraints are valid!\n"))
    return(invisible(NULL))
  }

  return(invalid)
}
