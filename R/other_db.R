#' Get Other Labels
#'
#' Retrieves text labels for questions in the kobo survey that correspond to "other" responses.
#'
#' @param kobo_survey A dataframe containing the Kobo survey sheet.
#' @return A dataframe containing the corresponding other labels.
#' @export
get_other_labels <- function(kobo_survey) {
  other_labels <- kobo_survey %>%
    filter((type == "text" & str_ends(name, "_other"))) %>%
    mutate(ref_question = as.character(lapply(relevant, get_ref_question))) %>%
    mutate(ref_question = ifelse(is.na(ref_question), name, ref_question)) %>%
    select(name, ref_question) %>%
    left_join(
      select(kobo_survey, ref_question = name, full_label = label),
      by = "ref_question"
    )

  cat(green(" - SAVING (./resources/labels_questions_others.xlsx) ... \n"))

  if (!dir.exists("resources")) {
    dir.create("resources", recursive = TRUE)
  }
  write.xlsx(
    other_labels,
    "resources/labels_questions_others.xlsx",
    overwrite = T
  )

  return(other_labels)
}
#' Get Other DB
#'
#' Processes the 'other_labels' alongside the survey inputs to map out the available choices for recoding.
#'
#' @param kobo_survey A dataframe representing the Kobo survey.
#' @param kobo_choices A dataframe representing the Kobo choices.
#' @param other_labels A dataframe retrieved from `get_other_labels`.
#' @return A dataframe representing the mapping required for other responses database.
#' @export
get_other_db <- function(kobo_survey, kobo_choices, other_labels) {
  # generate other_db
  other_db <- other_labels %>%
    left_join(
      select(kobo_survey, name, q_type, list_name),
      by = c("ref_question" = "name")
    ) %>%
    left_join(select(kobo_survey, name, relevant), by = "name") %>%
    mutate(
      option_other = str_replace_all(str_extract(relevant, "\'.*\'"), "'", "")
    ) %>%
    select(-relevant)

  # remove all option_other from choices
  kobo_choices_sub <- filter(kobo_choices, list_name %in% other_db$list_name)

  for (r in 1:nrow(other_db)) {
    if (!is.na(other_db$option_other[r])) {
      kobo_choices_sub <- kobo_choices_sub %>%
        filter(
          !(list_name == other_db$list_name[r] &
            name == other_db$option_other[r])
        )
    }
  }
  # add list of available choices
  other_db <- other_db %>%
    left_join(
      select(kobo_choices_sub, list_name, label = "label") %>%
        group_by(list_name) %>%
        summarise(num_choices = n(), choices = paste0(label, collapse = ";;")),
      by = "list_name"
    )

  return(other_db)
}
