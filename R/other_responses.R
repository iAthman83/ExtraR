#' Detect Select Multiple Separator Dynamically
#'
#' Scans the columns of the active clean_data/raw_data dataset to determine whether
#' the separator for select_multiple choice columns is a dot (\code{.}) or a slash (\code{/}).
#'
#' @param ref_name The name of the select_multiple question.
#' @param clean_data The active dataset. If \code{NULL}, looks for \code{clean_data} or \code{raw_data} in the environment.
#' @return A character string, either \code{"."} or \code{"/"}.
#' @keywords internal
detect_select_multiple_separator <- function(ref_name, clean_data = NULL) {
  # Resolve clean_data dynamically if NULL
  if (is.null(clean_data)) {
    if (exists("clean_data", envir = parent.frame())) {
      clean_data <- get("clean_data", envir = parent.frame())
    } else if (exists("clean_data", envir = .GlobalEnv)) {
      clean_data <- get("clean_data", envir = .GlobalEnv)
    } else if (exists("raw_data", envir = parent.frame())) {
      clean_data <- get("raw_data", envir = parent.frame())
    } else if (exists("raw_data", envir = .GlobalEnv)) {
      clean_data <- get("raw_data", envir = .GlobalEnv)
    }
  }

  if (is.null(clean_data) || !is.data.frame(clean_data)) {
    return("/") # Default fallback
  }

  # Look for columns starting with "ref_name."
  has_dot <- any(stringr::str_starts(colnames(clean_data), paste0(ref_name, ".")))
  if (has_dot) {
    return(".")
  }
  
  return("/") # Default Kobo format
}

################################################################################

#' Add Removal of Other Response to Cleaning Log
#'
#' Logs the removal of an "other" response string and handles the clean-up of its
#' associated question value. For single-choice questions, it sets the main selection to \code{NA}.
#' For multiple-choice questions, it removes the other choice from the choice string and
#' sets the corresponding binary option column to \code{"0"}.
#'
#' @param x A list or data frame row representing the "other" response record. 
#'   Must contain \code{uuid}, \code{name} (other question name), \code{ref_name} (main question name), 
#'   \code{ref_type} (main question type), and either \code{response_ar} or \code{response_en}.
#' @return NULL. Updates the global variable \code{cleaning_log_other} in-place.
#' @export
add_to_cleaning_log_other_remove <- function(x) {
  change_type <- "Removing other response"
  
  # Resolve dependencies dynamically
  if (!exists("other_db", envir = parent.frame()) && !exists("other_db", envir = .GlobalEnv)) {
    stop("other_db dataframe is not available in the environment.")
  }
  other_db <- if (exists("other_db", envir = parent.frame())) get("other_db", envir = parent.frame()) else get("other_db", envir = .GlobalEnv)

  if (!exists("clean_data", envir = parent.frame()) && !exists("clean_data", envir = .GlobalEnv)) {
    if (exists("raw_data", envir = parent.frame())) {
      clean_data <- get("raw_data", envir = parent.frame())
    } else if (exists("raw_data", envir = .GlobalEnv)) {
      clean_data <- get("raw_data", envir = .GlobalEnv)
    } else {
      stop("clean_data (or raw_data) dataframe is not available in the environment.")
    }
  } else {
    clean_data <- if (exists("clean_data", envir = parent.frame())) get("clean_data", envir = parent.frame()) else get("clean_data", envir = .GlobalEnv)
  }

  if (!exists("cleaning_log_other", envir = .GlobalEnv)) {
    cleaning_log_other <<- data.frame(
      uuid = character(),
      question = character(),
      change_type = character(),
      old_value = character(),
      new_value = character(),
      stringsAsFactors = FALSE
    )
  }

  # Load option other safely
  option_other_match <- other_db$option_other[other_db$name == x$name]
  option_other <- if (length(option_other_match) > 0 && !is.na(option_other_match[1])) option_other_match[1] else "other"
  
  # Dynamically detect separator (. or /)
  sep <- detect_select_multiple_separator(x$ref_name, clean_data)
  var_option_other <- paste0(x$ref_name, sep, option_other)
  
  # Determine which response column to use
  response_val <- NA_character_
  if ("response_ar" %in% names(x)) {
    response_val <- as.character(x$response_ar)
  } else if ("response_en" %in% names(x)) {
    response_val <- as.character(x$response_en)
  } else if ("response" %in% names(x)) {
    response_val <- as.character(x$response)
  }
  
  # Remove text of the response
  df <- data.frame(
    uuid = as.character(x$uuid), 
    question = as.character(x$name), 
    change_type = change_type, 
    old_value = response_val, 
    new_value = NA_character_,
    stringsAsFactors = FALSE
  )
  cleaning_log_other <<- rbind(cleaning_log_other, df)
  
  # Remove relative entries based on type
  if (x$ref_type == "select_one") {
    df <- data.frame(
      uuid = as.character(x$uuid), 
      question = as.character(x$ref_name), 
      change_type = change_type, 
      old_value = option_other, 
      new_value = NA_character_,
      stringsAsFactors = FALSE
    )
    cleaning_log_other <<- rbind(cleaning_log_other, df)
  } else if (x$ref_type == "select_multiple") {
    old_concat_value <- get_value_from_uuid(x$uuid, x$ref_name)
    new_concat_value <- remove_choice(old_concat_value, option_other)
    new_concat_value <- ifelse(new_concat_value == "", NA_character_, new_concat_value)
    
    df <- data.frame(
      uuid = as.character(x$uuid), 
      question = as.character(x$ref_name), 
      change_type = change_type,
      old_value = old_concat_value, 
      new_value = new_concat_value,
      stringsAsFactors = FALSE
    )
    cleaning_log_other <<- rbind(cleaning_log_other, df)
    
    if (is.na(new_concat_value)) {
      # Set all option columns to NA
      cols <- colnames(clean_data)[stringr::str_starts(colnames(clean_data), paste0(x$ref_name, sep))]
      match_rows <- which(clean_data$uuid == x$uuid)
      
      if (length(match_rows) == 0) {
        old_values <- rep(NA_character_, length(cols))
      } else {
        old_values <- as.character(clean_data[match_rows[1], cols])
      }
      
      if (length(cols) != length(old_values)) {
        stop("cols and old_values have different lengths")
      }
      
      for (i in seq_along(cols)) {
        df <- data.frame(
          uuid = as.character(x$uuid), 
          question = cols[i], 
          change_type = change_type, 
          old_value = old_values[i], 
          new_value = NA_character_,
          stringsAsFactors = FALSE
        )
        cleaning_log_other <<- rbind(cleaning_log_other, df)
      }
    } else {
      # Set var_option_other to "0"
      df <- data.frame(
        uuid = as.character(x$uuid), 
        question = var_option_other, 
        change_type = change_type,
        old_value = "1", 
        new_value = "0",
        stringsAsFactors = FALSE
      )
      cleaning_log_other <<- rbind(cleaning_log_other, df)
    }
  } else if (x$ref_type == "text") {
    df <- data.frame(
      uuid = as.character(x$uuid), 
      question = as.character(x$ref_name), 
      change_type = change_type, 
      old_value = response_val, 
      new_value = NA_character_,
      stringsAsFactors = FALSE
    )
    cleaning_log_other <<- rbind(cleaning_log_other, df)
  }
}

################################################################################

#' Add Recoding of Other Response to Cleaning Log
#'
#' Routes a recoding action to the appropriate handler based on whether the referenced 
#' question is a \code{select_one} or \code{select_multiple}.
#'
#' @param x A list or data frame row representing the "other" response record.
#' @return NULL. Updates the global variable \code{cleaning_log_other} in-place.
#' @export
add_to_cleaning_log_other_recode <- function(x) {
  if (x$ref_type[1] == "select_one") {
    add_to_cleaning_log_other_recode_one(x)
  } else if (x$ref_type[1] == "select_multiple") {
    add_to_cleaning_log_other_recode_multiple(x)
  }
}

################################################################################

#' Add Recoding of select_one Other Response to Cleaning Log
#'
#' Logs the recoding of a \code{select_one} question's "other" response. Looks up the
#' choice name from the selected label in the choices sheet, logs the removal of the 
#' text response, and updates the parent selection.
#'
#' @param x A list or data frame row representing the "other" response record.
#' @return NULL. Updates the global variable \code{cleaning_log_other} in-place.
#' @export
add_to_cleaning_log_other_recode_one <- function(x) {
  change_type <- "Recoding other response"
  
  if (!exists("cleaning_log_other", envir = .GlobalEnv)) {
    cleaning_log_other <<- data.frame(
      uuid = character(),
      question = character(),
      change_type = character(),
      old_value = character(),
      new_value = character(),
      stringsAsFactors = FALSE
    )
  }

  # Determine which response column to use
  response_val <- NA_character_
  if ("response_ar" %in% names(x)) {
    response_val <- as.character(x$response_ar)
  } else if ("response_en" %in% names(x)) {
    response_val <- as.character(x$response_en)
  } else if ("response" %in% names(x)) {
    response_val <- as.character(x$response)
  }

  # Remove text of the response
  df <- data.frame(
    uuid = as.character(x$uuid), 
    question = as.character(x$name), 
    change_type = change_type, 
    old_value = response_val, 
    new_value = NA_character_,
    stringsAsFactors = FALSE
  )
  cleaning_log_other <<- rbind(cleaning_log_other, df)
  
  # Recode choice
  new_value <- get_name_from_label(x$list_name, x$existing_other)
  
  if (is.null(new_value) || length(new_value) == 0 || is.na(new_value) || new_value == "") {
    warning(paste0("Choice '", x$existing_other, "' not found in list '", x$list_name, "'. Recoding to 'NA'."))
    new_value <- NA_character_
  }
  
  df <- data.frame(
    uuid = as.character(x$uuid), 
    question = as.character(x$ref_name), 
    change_type = change_type,
    old_value = get_value_from_uuid(x$uuid, x$ref_name), 
    new_value = new_value,
    stringsAsFactors = FALSE
  )
  cleaning_log_other <<- rbind(cleaning_log_other, df)
}

################################################################################

#' Add Recoding of select_multiple Other Response to Cleaning Log
#'
#' Logs the recoding of a \code{select_multiple} question's "other" response. Looks up the
#' choice name(s) for the selected labels, removes the "other" selection, adds the new
#' selections, and updates the global cleaning log.
#'
#' @param x A list or data frame row representing the "other" response record.
#' @return NULL. Updates the global variable \code{cleaning_log_other} in-place.
#' @export
add_to_cleaning_log_other_recode_multiple <- function(x) {
  change_type <- "Recoding other response"

  # Resolve dependencies dynamically
  if (!exists("other_db", envir = parent.frame()) && !exists("other_db", envir = .GlobalEnv)) {
    stop("other_db dataframe is not available in the environment.")
  }
  other_db <- if (exists("other_db", envir = parent.frame())) get("other_db", envir = parent.frame()) else get("other_db", envir = .GlobalEnv)

  if (!exists("cleaning_log_other", envir = .GlobalEnv)) {
    cleaning_log_other <<- data.frame(
      uuid = character(),
      question = character(),
      change_type = character(),
      old_value = character(),
      new_value = character(),
      stringsAsFactors = FALSE
    )
  }

  # Get option other safely
  option_other_match <- other_db$option_other[other_db$name == x$name]
  option_other <- if (length(option_other_match) > 0 && !is.na(option_other_match[1])) option_other_match[1] else "other"
  
  # Resolve separator (. or /)
  sep <- detect_select_multiple_separator(x$ref_name)
  var_option_other <- paste0(x$ref_name, sep, option_other)
  
  # Determine which response column to use
  response_val <- NA_character_
  if ("response_ar" %in% names(x)) {
    response_val <- as.character(x$response_ar)
  } else if ("response_en" %in% names(x)) {
    response_val <- as.character(x$response_en)
  } else if ("response" %in% names(x)) {
    response_val <- as.character(x$response)
  }

  # Remove text of the response
  df <- data.frame(
    uuid = as.character(x$uuid), 
    question = as.character(x$name), 
    change_type = change_type, 
    old_value = response_val, 
    new_value = NA_character_,
    stringsAsFactors = FALSE
  )
  cleaning_log_other <<- rbind(cleaning_log_other, df)
  
  # Set option other to "0" and selected choices to "1" (if not already "1")
  if (is.null(x$existing_other) || is.na(x$existing_other) || x$existing_other == "") {
    choices <- character(0)
  } else {
    choices_raw <- stringr::str_split(x$existing_other, ";")[[1]]
    choices <- unlist(lapply(choices_raw, function(c) {
      get_name_from_label(x$list_name, trimws(c))
    }))
    choices <- choices[!is.na(choices) & choices != ""]
  }
  
  if (option_other %in% choices) {
    warning(paste0(x$name, ": adding again the 'other' option"))
  }
  
  ref_question_match <- other_db$ref_question[other_db$name == x$name]
  ref_name <- if (length(ref_question_match) > 0) ref_question_match[1] else x$ref_name
  
  el <- list(uuid = x$uuid, ref_name = ref_name, change_type = change_type)
  recoded_entries <- select_multiple_add_remove(el, to_remove = c(option_other), to_add = choices)
  
  cleaning_log_other <<- rbind(cleaning_log_other, recoded_entries)
}

################################################################################

#' Generate Cleaning Log entries for select_multiple Modification
#'
#' Computes the necessary cleaning log entries when choices are added and/or removed 
#' from a \code{select_multiple} question. Automatically updates both the concatenated 
#' value column and the individual binary option columns (e.g. \code{question/choice}).
#'
#' @param el A list containing \code{uuid}, \code{ref_name} (main question name), and \code{change_type}.
#' @param to_remove A character vector of choice names to remove.
#' @param to_add A character vector of choice names to add.
#' @param exclusive_options A character vector of choice names that are exclusive 
#'   (e.g., "none", "dont_know"). If selected, all other choices will be deselected.
#' @return A data frame containing cleaning log entries with columns \code{uuid}, \code{question}, 
#'   \code{old_value}, \code{new_value}, and \code{change_type}.
#' @export
select_multiple_add_remove <- function(el, to_remove, to_add = c(), 
                                       exclusive_options = c("none", "none_of_the_above", "dont_know", "dk", "pnd", "no_other_choices")) {
  # Resolve dependencies dynamically
  if (!exists("clean_data", envir = parent.frame()) && !exists("clean_data", envir = .GlobalEnv)) {
    if (exists("raw_data", envir = parent.frame())) {
      clean_data <- get("raw_data", envir = parent.frame())
    } else if (exists("raw_data", envir = .GlobalEnv)) {
      clean_data <- get("raw_data", envir = .GlobalEnv)
    } else {
      stop("clean_data (or raw_data) dataframe is not available in the environment.")
    }
  } else {
    clean_data <- if (exists("clean_data", envir = parent.frame())) get("clean_data", envir = parent.frame()) else get("clean_data", envir = .GlobalEnv)
  }

  # Dynamically detect separator (. or /)
  sep <- detect_select_multiple_separator(el$ref_name, clean_data)

  # Get column names for the options
  cols <- colnames(clean_data)[stringr::str_starts(colnames(clean_data), paste0(el$ref_name, sep))]
  
  # Generate cleaning log
  cl <- data.frame(
    uuid = character(),
    question = character(),
    old_value = character(),
    new_value = character(),
    stringsAsFactors = FALSE
  )
  
  current_val <- get_value_from_uuid(el$uuid, el$ref_name)
  
  if (is.na(current_val) && length(to_remove) > 0) {
    stop("Cannot remove choices when the current value in the dataset is NA.")
  }
  
  if (is.na(current_val)) {
    #---------------------------------------------------------------------------
    # CASE 1) old value is NA
    if (length(exclusive_options) > 0 && any(exclusive_options %in% to_add)) {
      stop("Recoding of exclusive options from NA is not implemented yet.")
    }
    
    concat <- ""
    for (col in cols) {
      choice <- stringr::str_split(col, stringr::fixed(sep))[[1]][2]
      if (choice %in% to_add) {
        new_value <- "1"
        concat <- add_choice(concat, choice)
      } else {
        new_value <- "0"
      }
      cl <- rbind(cl, data.frame(
        uuid = as.character(el$uuid), 
        question = col, 
        old_value = NA_character_, 
        new_value = new_value,
        stringsAsFactors = FALSE
      ))
    }
    
    if (concat == "") {
      stop("No options were added to concat value.")
    }
    
    cl <- rbind(cl, data.frame(
      uuid = as.character(el$uuid), 
      question = as.character(el$ref_name), 
      old_value = NA_character_, 
      new_value = trimws(concat),
      stringsAsFactors = FALSE
    ))
  } else {
    #---------------------------------------------------------------------------
    # CASE 2) old value is not NA
    old_concat <- as.character(current_val)
    if (old_concat == "") {
      stop("Concatenated select_multiple value is empty.")
    }
    new_concat <- old_concat
    
    # Remove options
    if (length(to_remove) > 0) {
      for (choice in to_remove) {
        cl <- rbind(cl, data.frame(
          uuid = as.character(el$uuid), 
          question = paste0(el$ref_name, sep, choice),
          old_value = "1", 
          new_value = "0",
          stringsAsFactors = FALSE
        ))
        new_concat <- remove_choice(new_concat, choice)
      }
    }
    
    # Add options
    if (length(to_add) > 0) {
      if (any(exclusive_options %in% to_add)) {
        print(paste0("Recoding exclusive option: ", el$uuid, " --> ", el$ref_name, " --> ", to_add[1]))
        if (length(to_add) > 1) {
          stop("Cannot select an exclusive option together with other choices.")
        }
        
        cl <- data.frame(
          uuid = character(),
          question = character(),
          old_value = character(),
          new_value = character(),
          stringsAsFactors = FALSE
        )
        
        for (col in cols) {
          option <- stringr::str_split(col, stringr::fixed(sep))[[1]][2]
          old_value <- as.character(get_value_from_uuid(el$uuid, col))
          
          if (option == to_add[1]) {
            cl <- rbind(cl, data.frame(
              uuid = as.character(el$uuid), 
              question = col, 
              old_value = "0", 
              new_value = "1",
              stringsAsFactors = FALSE
            ))
            new_concat <- to_add[1]
          } else if (!is.na(old_value) && old_value == "1") {
            cl <- rbind(cl, data.frame(
              uuid = as.character(el$uuid), 
              question = col, 
              old_value = "1", 
              new_value = "0",
              stringsAsFactors = FALSE
            ))
          }
        }
      } else {
        for (choice in to_add) {
          old_value <- as.character(get_value_from_uuid(el$uuid, paste0(el$ref_name, sep, choice)))
          if (!is.na(old_value) && old_value == "0") {
            cl <- rbind(cl, data.frame(
              uuid = as.character(el$uuid), 
              question = paste0(el$ref_name, sep, choice),
              old_value = "0", 
              new_value = "1",
              stringsAsFactors = FALSE
            ))
            new_concat <- add_choice(new_concat, choice)
          }
        }
      }
    }
    
    #---------------------------------------------------------------------------
    # Either update the concatenated column or set all option columns to NA if new_concat is empty
    if (new_concat != "" && new_concat != old_concat) {
      cl <- rbind(cl, data.frame(
        uuid = as.character(el$uuid), 
        question = as.character(el$ref_name), 
        old_value = old_concat, 
        new_value = trimws(new_concat),
        stringsAsFactors = FALSE
      ))
    } else if (new_concat == "") {
      cl <- data.frame(
        uuid = character(),
        question = character(),
        old_value = character(),
        new_value = character(),
        stringsAsFactors = FALSE
      )
      
      for (col in cols) {
        cl <- rbind(cl, data.frame(
          uuid = as.character(el$uuid), 
          question = col, 
          old_value = as.character(get_value_from_uuid(el$uuid, col)), 
          new_value = NA_character_,
          stringsAsFactors = FALSE
        ))
      }
      
      cl <- rbind(cl, data.frame(
        uuid = as.character(el$uuid), 
        question = as.character(el$ref_name), 
        old_value = old_concat, 
        new_value = NA_character_,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  if (nrow(cl) > 0) {
    cl$change_type <- el$change_type
  } else {
    cl <- data.frame(
      uuid = character(), 
      question = character(), 
      old_value = character(), 
      new_value = character(), 
      change_type = character(),
      stringsAsFactors = FALSE
    )
  }
  
  return(cl)
}
