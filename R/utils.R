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

#' Load Required Packages for ExtraR
#'
#' This function installs (if necessary) and loads all packages required
#' for the ExtraR package to run. It uses the `pak` package for efficient
#' installation and dependency management.
#'
#' @export
load_extrar_packages <- function() {
  # List of core packages required
  core_pkgs <- c(
    "dplyr",
    "stringr",
    "tidyr",
    "openxlsx",
    "srvyr",
    "uuid",
    "rlang",
    "crayon",
    "readxl",
    "purrr",
    "httr",
    "jsonlite",
    "keyring",
    "magrittr"
  )
  
  # List of IMPACT specific packages from github
  impact_pkgs <- c(
    "impact-initiatives/cleaningtools",
    "impact-initiatives/analysistools",
    "impact-initiatives/presentresults",
    "impact-initiatives/addindicators"
  )
  
  # Install pak if not installed
  if (!requireNamespace("pak", quietly = TRUE)) {
    install.packages("pak")
  }
  
  # Combine packages
  all_pkgs_to_install <- c(core_pkgs, impact_pkgs)
  
  # Use pak to install any missing packages
  cat(crayon::yellow("Checking and installing missing packages using 'pak'...\n"))
  pak::pkg_install(all_pkgs_to_install)
  
  # Extract just the package names for loading
  impact_pkg_names <- gsub(".*/", "", impact_pkgs)
  all_pkg_names <- c(core_pkgs, impact_pkg_names)
  
  # Load all packages
  cat(crayon::yellow("Loading required packages...\n"))
  invisible(lapply(all_pkg_names, function(pkg) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }))
  
  cat(crayon::green("All required packages loaded successfully!\n"))
}

#' Get Choice Name from Choice Label
#'
#' Looks up the name for a given choice label within a specific list
#' in the Kobo choices sheet. Automatically looks up the choices sheet
#' in the environment if not explicitly provided.
#'
#' @param list_name The \code{list_name} value to filter by.
#' @param label The choice \code{label} value to look up.
#' @param kobo_choices Optional dataframe containing the Kobo choices sheet.
#'   If \code{NULL}, looks for \code{kobo_choices} in the environment.
#' @return The name string for the matching choice, or \code{NA} if not found.
#' @export
get_name_from_label <- function(list_name, label, kobo_choices = NULL) {
  # Resolve kobo_choices dynamically if NULL
  if (is.null(kobo_choices)) {
    if (exists("kobo_choices", envir = parent.frame())) {
      kobo_choices <- get("kobo_choices", envir = parent.frame())
    } else if (exists("kobo_choices", envir = .GlobalEnv)) {
      kobo_choices <- get("kobo_choices", envir = .GlobalEnv)
    } else {
      stop("kobo_choices dataframe is not provided and was not found in the environment.")
    }
  }

  if (!is.data.frame(kobo_choices) || !all(c("list_name", "label", "name") %in% colnames(kobo_choices))) {
    stop("kobo_choices must be a dataframe containing 'list_name', 'label', and 'name' columns.")
  }

  match_idx <- which(kobo_choices$list_name == list_name & kobo_choices$label == label)
  if (length(match_idx) == 0) {
    return(NA_character_)
  }
  return(kobo_choices$name[match_idx[1]])
}

################################################################################

#' Get Choice Label from Choice Name
#'
#' Looks up the label for a given choice name within a specific list
#' in the Kobo choices sheet. Automatically looks up the choices sheet
#' in the environment if not explicitly provided.
#'
#' @param list.name The \code{list_name} value to filter by.
#' @param name The choice \code{name} value to look up.
#' @param kobo_choices Optional dataframe containing the Kobo choices sheet.
#'   If \code{NULL}, looks for \code{kobo_choices} in the environment.
#' @return The label string for the matching choice, or \code{NA} if not found.
#' @export
get_label_from_name <- function(list.name, name, kobo_choices = NULL) {
  # Resolve kobo_choices dynamically if NULL
  if (is.null(kobo_choices)) {
    if (exists("kobo_choices", envir = parent.frame())) {
      kobo_choices <- get("kobo_choices", envir = parent.frame())
    } else if (exists("kobo_choices", envir = .GlobalEnv)) {
      kobo_choices <- get("kobo_choices", envir = .GlobalEnv)
    } else {
      stop("kobo_choices dataframe is not provided and was not found in the environment.")
    }
  }

  if (!is.data.frame(kobo_choices) || !all(c("list_name", "label", "name") %in% colnames(kobo_choices))) {
    stop("kobo_choices must be a dataframe containing 'list_name', 'label', and 'name' columns.")
  }

  match_idx <- which(kobo_choices$list_name == list.name & kobo_choices$name == name)
  if (length(match_idx) == 0) {
    return(NA_character_)
  }
  return(kobo_choices$label[match_idx[1]])
}

################################################################################

#' Get Column Value by UUID
#'
#' Retrieves a value from a specific column for a given UUID. If the UUID
#' contains an underscore (indicating a loop/roster entry), it searches the loop
#' dataset; otherwise it searches the main dataset. This function is fully vectorized,
#' handles missing/empty values gracefully, and automatically looks up datasets
#' in the environment if not explicitly provided.
#'
#' @param uuid A character vector of UUIDs to look up.
#' @param column The column name (scalar character) to retrieve the value from.
#' @param raw_data The main raw dataset dataframe. If \code{NULL}, looks for \code{raw_data} in the environment.
#' @param raw_roster Optional roster dataset dataframe. If \code{NULL}, looks for \code{raw_roster} or \code{raw_loop} in the environment.
#' @param raw_loop Optional loop dataset dataframe. Synonym for \code{raw_roster}.
#' @return A vector of values from the specified column for the matching UUIDs.
#' @export
get_value_from_uuid <- function(uuid, column, raw_data = NULL, raw_roster = NULL, raw_loop = NULL) {
  # 1. Validate column argument
  if (missing(column) || is.null(column) || is.na(column) || length(column) != 1 || !is.character(column)) {
    stop("The 'column' parameter must be a single, non-empty character string.")
  }

  # 2. Resolve raw_data
  if (is.null(raw_data)) {
    if (exists("raw_data", envir = parent.frame())) {
      raw_data <- get("raw_data", envir = parent.frame())
    } else if (exists("raw_data", envir = .GlobalEnv)) {
      raw_data <- get("raw_data", envir = .GlobalEnv)
    } else {
      stop("raw_data dataframe is not provided and was not found in the environment.")
    }
  }

  if (!is.data.frame(raw_data)) {
    stop("raw_data must be a data frame.")
  }

  # 3. Resolve loop/roster dataset
  loop_dataset <- NULL
  if (!is.null(raw_roster)) {
    loop_dataset <- raw_roster
  } else if (!is.null(raw_loop)) {
    loop_dataset <- raw_loop
  } else {
    # Look in parent frame or global environment for raw_roster then raw_loop
    if (exists("raw_roster", envir = parent.frame())) {
      loop_dataset <- get("raw_roster", envir = parent.frame())
    } else if (exists("raw_loop", envir = parent.frame())) {
      loop_dataset <- get("raw_loop", envir = parent.frame())
    } else if (exists("raw_roster", envir = .GlobalEnv)) {
      loop_dataset <- get("raw_roster", envir = .GlobalEnv)
    } else if (exists("raw_loop", envir = .GlobalEnv)) {
      loop_dataset <- get("raw_loop", envir = .GlobalEnv)
    }
  }

  # 4. Helper function to get value for a single UUID
  get_single_value <- function(id, col) {
    if (is.null(id) || is.na(id) || id == "") {
      return(NA)
    }

    # Determine if it's a loop entry
    is_loop <- stringr::str_detect(id, "_")

    if (is_loop) {
      if (is.null(loop_dataset)) {
        stop("UUID '", id, "' indicates a loop/roster entry, but no raw_roster or raw_loop dataset is available.")
      }
      if (!"uuid" %in% colnames(loop_dataset)) {
        stop("The loop/roster dataset does not have a 'uuid' column.")
      }
      if (!col %in% colnames(loop_dataset)) {
        warning("Column '", col, "' not found in the loop/roster dataset. Returning NA.")
        return(NA)
      }
      val <- loop_dataset[[col]][loop_dataset$uuid == id]
      if (length(val) == 0) return(NA)
      return(val[1]) # Return the first match if duplicate UUIDs exist
    } else {
      if (!"uuid" %in% colnames(raw_data)) {
        stop("The raw_data dataset does not have a 'uuid' column.")
      }
      if (!col %in% colnames(raw_data)) {
        warning("Column '", col, "' not found in raw_data. Returning NA.")
        return(NA)
      }
      val <- raw_data[[col]][raw_data$uuid == id]
      if (length(val) == 0) return(NA)
      return(val[1])
    }
  }

  # 5. Vectorized execution
  results <- sapply(uuid, function(id) get_single_value(id, column), USE.NAMES = FALSE)
  return(results)
}

################################################################################

#' Add a Choice to a Space-Separated Select Multiple String
#'
#' Vector-safe utility to add a specific choice to a space-separated string of choices.
#' Automatically handles duplicate entries, empty strings, and missing values.
#'
#' @param concat_value A character vector containing space-separated choices.
#' @param choice A character vector of the choice(s) to add.
#' @return A character vector with the choice added.
#' @export
add_choice <- function(concat_value, choice) {
  add_single <- function(cv, ch) {
    if (is.null(ch) || is.na(ch) || ch == "") {
      return(if (is.null(cv) || is.na(cv)) NA_character_ else as.character(cv))
    }
    if (is.null(cv) || is.na(cv) || cv == "") {
      return(as.character(ch))
    }
    l <- stringr::str_split(cv, "\\s+")[[1]]
    l <- sort(unique(c(l, ch)))
    l <- l[l != "" & !is.na(l)]
    if (length(l) == 0) return("")
    return(paste(l, collapse = " "))
  }

  # Ensure character inputs
  concat_value <- if (is.null(concat_value)) NA_character_ else as.character(concat_value)
  choice <- if (is.null(choice)) NA_character_ else as.character(choice)

  # Fully vectorized over both vectors
  res <- as.character(mapply(add_single, concat_value, choice, USE.NAMES = FALSE))
  return(res)
}

################################################################################

#' Remove a Choice from a Space-Separated Select Multiple String
#'
#' Vector-safe utility to remove a specific choice from a space-separated string of choices.
#' Automatically handles empty strings and missing values.
#'
#' @param concat_value A character vector containing space-separated choices.
#' @param choice A character vector of the choice(s) to remove.
#' @return A character vector with the choice removed.
#' @export
remove_choice <- function(concat_value, choice) {
  remove_single <- function(cv, ch) {
    if (is.null(ch) || is.na(ch) || ch == "") {
      return(if (is.null(cv) || is.na(cv)) NA_character_ else as.character(cv))
    }
    if (is.null(cv) || is.na(cv) || cv == "") {
      return("")
    }
    l <- stringr::str_split(cv, "\\s+")[[1]]
    l <- l[l != ch & l != "" & !is.na(l)]
    if (length(l) == 0) return("")
    return(paste(l, collapse = " "))
  }

  # Ensure character inputs
  concat_value <- if (is.null(concat_value)) NA_character_ else as.character(concat_value)
  choice <- if (is.null(choice)) NA_character_ else as.character(choice)

  # Fully vectorized over both vectors
  res <- as.character(mapply(remove_single, concat_value, choice, USE.NAMES = FALSE))
  return(res)
}

################################################################################

#' Extract Referenced Question Name from Relevance Expression
#'
#' Parses a Kobo relevance expression string to extract the referenced
#' question name enclosed in curly braces (e.g., \code{${question_name}}).
#' This function is fully vectorized.
#'
#' @param x A character vector containing Kobo relevance expressions.
#' @return A character vector of the extracted question names, or \code{NA} if not found.
#' @export
get_ref_question <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  return(stringr::str_extract(as.character(x), "(?<=\\{)[^}]+"))
}

################################################################################

#' Apply Kobo Labels
#'
#' Renames the analysis output options columns using a Kobo choices dictionary before a user can save the analysis.
#'
#' @param dataset The dataset to modify
#' @param column_name The name of the column to relabel
#' @param kobo_choices The choices dictionary from Kobo
#' @return The updated dataset
#' @export
apply_kobo_labels <- function(dataset, column_name, kobo_choices) {
  # 1. Check if the column actually exists in the dataset
  if (!column_name %in% colnames(dataset)) {
    stop(paste(
      "Error: The column",
      column_name,
      "does not exist in the dataset."
    ))
  }

  # 2. Create the dictionary from the choices sheet
  label_lookup <- kobo_choices %>%
    select(name, label) %>%
    mutate(
      name = as.character(name),
      label = as.character(label)
    ) %>%
    filter(!is.na(name) & name != "") %>%
    # Ensure there is only one entry per machine name globally
    distinct(name, .keep_all = TRUE)

  # Create a named vector: c("machine_name" = "Readable Label")
  dict_vector <- setNames(label_lookup$label, label_lookup$name)

  # 3. Apply the dictionary to the target column in the dataset
  dataset_updated <- dataset %>%
    mutate(
      # Temporarily ensure the column is a character vector for safe matching
      !!sym(column_name) := as.character(!!sym(column_name)),

      # Replace names with labels. coalesce() keeps the original value if no match is found.
      !!sym(column_name) := coalesce(
        dict_vector[!!sym(column_name)],
        !!sym(column_name)
      )
    )

  return(dataset_updated)
}
