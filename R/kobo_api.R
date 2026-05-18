#' Set Kobo API Token
#'
#' Interactively securely saves the Kobo API token using keyring.
#' @export
kobo_setup_token <- function() {
  token <- readline(prompt = "Enter your Kobo API token: ")
  if (token != "") {
    keyring::key_set_with_value("kobo_api_token", password = token)
    cat(crayon::green("Token successfully saved.\n"))
  } else {
    cat(crayon::red("No token provided. Operation cancelled.\n"))
  }
}

#' Get Kobo API Token
#'
#' Retrieves the Kobo API token from the secure credential store.
#' @export
kobo_get_token <- function() {
  tryCatch(
    {
      keyring::key_get("kobo_api_token")
    },
    error = function(e) {
      stop("Kobo API token not found. Please run kobo_setup_token() first.")
    }
  )
}

#' Trigger Kobo Export
#'
#' @param asset_id The Kobo asset ID
#' @param server_url The Kobo server URL
#' @return The export URL to poll
#' @export
kobo_export_trigger <- function(
  asset_id,
  server_url = "https://kobo.impact-initiatives.org"
) {
  token <- kobo_get_token()

  export_url <- paste0(server_url, "/api/v2/assets/", asset_id, "/exports/")

  payload <- list(
    type = "xls",
    lang = "_xml",
    fields_from_all_versions = TRUE,
    multiple_select = "both",
    group_sep = "/",
    hierarchy_in_labels = FALSE
  )

  req <- httr::POST(
    export_url,
    httr::add_headers(Authorization = paste("Token", token)),
    body = payload,
    encode = "json"
  )

  if (httr::status_code(req) %in% c(200, 201)) {
    res <- httr::content(req, "parsed")
    return(res$url)
  } else {
    stop(
      "Failed to trigger export. Status code: ",
      httr::status_code(req),
      " Response: ",
      httr::content(req, "text")
    )
  }
}

#' Poll Kobo Export Status
#'
#' @param export_url The export URL returned by `kobo_export_trigger`
#' @param sleep_time Seconds to wait between polling
#' @return The download URL when successful
#' @export
kobo_export_poll <- function(export_url, sleep_time = 5, max_retries = 60) {
  token <- kobo_get_token()

  cat(crayon::yellow("Polling export status...\n"))
  attempt <- 0
  repeat {
    attempt <- attempt + 1
    if (attempt > max_retries) {
      stop("Export timed out after ", max_retries, " attempts.")
    }

    req <- httr::GET(
      export_url,
      httr::add_headers(Authorization = paste("Token", token))
    )

    if (httr::status_code(req) == 200) {
      res <- httr::content(req, "parsed")
      # Kobo API v2 uses 'status' (lowercase values: complete, error, processing, created)
      status <- res$status %||% res$job_status

      if (is.null(status)) {
        # If neither field exists, dump the response keys for debugging
        stop(
          "Unexpected API response. Available fields: ",
          paste(names(res), collapse = ", ")
        )
      }

      status <- tolower(status)

      if (status %in% c("complete", "success")) {
        cat(crayon::green("Export successful.\n"))
        download_url <- res$result
        if (is.null(download_url)) {
          stop("Export completed but no download URL found in response.")
        }
        return(download_url)
      } else if (status %in% c("error", "failed")) {
        stop("Export failed on Kobo server.")
      }

      cat(crayon::yellow("."))
      Sys.sleep(sleep_time)
    } else {
      stop(
        "Failed to poll export status. Status code: ",
        httr::status_code(req),
        " Response: ",
        httr::content(req, "text")
      )
    }
  }
}

#' Download Kobo Data
#'
#' Main function to download data from Kobo.
#'
#' @param asset_id The Kobo asset ID
#' @param server_url The Kobo server URL (default: "https://kobo.impact-initiatives.org")
#' @param cache_dir Directory to save the downloaded file
#' @return The path to the downloaded Excel file
#' @export
kobo_download_data <- function(
  asset_id,
  server_url = "https://kobo.impact-initiatives.org",
  cache_dir = "data/"
) {
  # Ensure cache dir exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  cat(crayon::green(paste0(
    "--> Triggering export for asset: ",
    asset_id,
    "\n"
  )))
  export_url <- kobo_export_trigger(asset_id, server_url)

  download_url <- kobo_export_poll(export_url)

  # Ensure cache filename is unique per asset but overwrites old versions
  filename <- paste0("kobo_data_", asset_id, ".xlsx")
  file_path <- file.path(cache_dir, filename)

  cat(crayon::green(paste0("--> Downloading data to: ", file_path, "\n")))
  token <- kobo_get_token()

  # Download file
  req <- httr::GET(
    download_url,
    httr::add_headers(Authorization = paste("Token", token)),
    httr::write_disk(file_path, overwrite = TRUE)
  )

  if (httr::status_code(req) == 200) {
    cat(crayon::green("Download complete.\n"))
    return(file_path)
  } else {
    stop("Failed to download file. Status code: ", httr::status_code(req))
  }
}

#' Download Kobo Audit Files
#'
#' Downloads audit log files for each submission from the Kobo server.
#' Audit files track enumerator timing (how long each survey question took).
#' Uses token-based authentication via `kobo_get_token()`.
#' Already-downloaded audits are skipped automatically.
#'
#' @param df A dataframe of submissions (must contain `audit_URL` column).
#' @param uuid_column The name of the UUID column in the dataframe (default: `"_uuid"`).
#' @param audit_dir Directory to save the downloaded audit files.
#' @param zip_output If TRUE, creates a zip file of all audit folders after downloading (default: TRUE).
#' @return Invisibly returns the audit directory path.
#' @export
kobo_download_audits <- function(
  df,
  uuid_column = "uuid",
  audit_dir = "audits/",
  zip_output = TRUE
) {
  token <- kobo_get_token()

  # Validate inputs
  if (!uuid_column %in% names(df)) {
    stop("Column '", uuid_column, "' not found in the dataframe.")
  }
  if (!"audit_URL" %in% names(df)) {
    stop(
      "Column 'audit_URL' not found in the dataframe. ",
      "Make sure the Kobo form has audit logging enabled."
    )
  }

  # Ensure audit directory exists
  if (!dir.exists(audit_dir)) {
    dir.create(audit_dir, recursive = TRUE)
    cat(crayon::yellow(paste0("Created audit directory: ", audit_dir, "\n")))
  }

  # Skip UUIDs that are already downloaded
  already_downloaded <- list.dirs(
    audit_dir,
    full.names = FALSE,
    recursive = FALSE
  )
  df <- df[!df[[uuid_column]] %in% already_downloaded, ]

  # Filter out rows with missing audit URLs
  df <- df[!is.na(df[["audit_URL"]]) & df[["audit_URL"]] != "", ]

  if (nrow(df) == 0) {
    cat(crayon::green("All audit files are already downloaded.\n"))
  } else {
    cat(crayon::yellow(paste0(
      "--> Downloading ",
      nrow(df),
      " audit file(s)...\n"
    )))

    success_count <- 0
    fail_count <- 0

    for (i in seq_len(nrow(df))) {
      uuid_i <- df[[uuid_column]][i]
      url_i <- df[["audit_URL"]][i]

      cat(crayon::yellow(paste0(
        "  [",
        i,
        "/",
        nrow(df),
        "] ",
        uuid_i,
        " ... "
      )))

      tryCatch(
        {
          req <- httr::GET(
            url_i,
            httr::add_headers(Authorization = paste("Token", token)),
            httr::timeout(120)
          )

          if (httr::status_code(req) != 200) {
            cat(crayon::red(paste0("HTTP ", httr::status_code(req), "\n")))
            fail_count <- fail_count + 1
            next
          }

          audit_content <- httr::content(req, "text", encoding = "UTF-8")

          # Validate that we got actual audit data (not an error message)
          if (
            is.na(audit_content) ||
              audit_content == "" ||
              grepl("Attachment not found", audit_content, fixed = TRUE)
          ) {
            cat(crayon::red("No audit data found.\n"))
            fail_count <- fail_count + 1
            next
          }

          # Save the audit file
          uuid_dir <- file.path(audit_dir, uuid_i)
          dir.create(uuid_dir, showWarnings = FALSE, recursive = TRUE)
          audit_path <- file.path(uuid_dir, "audit.csv")

          # Try to parse as proper CSV, otherwise save raw
          parsed <- tryCatch(
            {
              utils::read.csv(text = audit_content, stringsAsFactors = FALSE)
            },
            error = function(e) NULL
          )

          if (!is.null(parsed) && nrow(parsed) > 0) {
            utils::write.csv(parsed, audit_path, row.names = FALSE)
          } else {
            writeLines(audit_content, audit_path)
          }

          cat(crayon::green("OK\n"))
          success_count <- success_count + 1
        },
        error = function(e) {
          cat(crayon::red(paste0("Error: ", e$message, "\n")))
          fail_count <<- fail_count + 1
        }
      )
    }

    cat(crayon::green(paste0("\nDone! ", success_count, " downloaded")))
    if (fail_count > 0) {
      cat(crayon::red(paste0(", ", fail_count, " failed")))
    }
    cat("\n")
  }

  # Zip audit files if requested
  if (zip_output) {
    audit_folders <- list.dirs(audit_dir, full.names = TRUE, recursive = FALSE)
    if (length(audit_folders) > 0) {
      zip_path <- paste0(
        tools::file_path_sans_ext(sub("/$", "", audit_dir)),
        ".zip"
      )
      cat(crayon::yellow(paste0(
        "--> Zipping audit files to: ",
        zip_path,
        "\n"
      )))
      zip::zip(
        zipfile = zip_path,
        files = audit_folders,
        mode = "cherry-pick"
      )
      cat(crayon::green("Zip file created successfully.\n"))
    } else {
      cat(crayon::yellow("No audit folders found to zip.\n"))
    }
  }

  return(invisible(audit_dir))
}
