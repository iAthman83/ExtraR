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
#' @param max_retries Maximum number of retries before timing out
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
#' @param filename The name of the downloaded file (default: "kobo_data_<asset_id>.xlsx")
#' @return The path to the downloaded Excel file
#' @export
kobo_download_data <- function(
  asset_id,
  server_url = "https://kobo.impact-initiatives.org",
  cache_dir = "data/",
  filename = paste0("kobo_data_", asset_id, ".xlsx")
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
#' Fetches audit URLs directly from the Kobo JSON API, so it works
#' regardless of how the main data was downloaded.
#' Already-downloaded audits are skipped automatically.
#'
#' @param asset_id The Kobo asset ID.
#' @param server_url The Kobo server URL.
#' @param audit_dir Directory to save the downloaded audit files.
#' @param zip_output If TRUE, creates a zip file of all audit folders after downloading (default: TRUE).
#' @return Invisibly returns the audit directory path.
#' @export
kobo_download_audits <- function(
  asset_id,
  server_url = "https://kobo.impact-initiatives.org",
  audit_dir = "audits/",
  zip_output = TRUE
) {
  token <- kobo_get_token()

  # Ensure audit directory exists
  if (!dir.exists(audit_dir)) {
    dir.create(audit_dir, recursive = TRUE)
    cat(crayon::yellow(paste0("Created audit directory: ", audit_dir, "\n")))
  }

  # Fetch submission data from the JSON API (includes audit URLs)
  cat(crayon::yellow("--> Fetching submission data from Kobo API...\n"))
  data_url <- paste0(server_url, "/api/v2/assets/", asset_id, "/data.json")

  all_results <- list()
  next_url <- data_url

  # Paginate through all results

  while (!is.null(next_url)) {
    req <- httr::GET(
      next_url,
      httr::add_headers(Authorization = paste("Token", token))
    )

    if (httr::status_code(req) != 200) {
      stop(
        "Failed to fetch submission data. Status code: ",
        httr::status_code(req)
      )
    }

    res <- httr::content(req, "parsed")
    all_results <- c(all_results, res$results)
    next_url <- res$`next`
  }

  cat(crayon::green(paste0(
    "Found ", length(all_results), " submission(s).\n"
  )))

  if (length(all_results) == 0) {
    cat(crayon::yellow("No submissions found.\n"))
    return(invisible(audit_dir))
  }

  # Extract UUID and audit URL from each submission
  audit_info <- lapply(all_results, function(submission) {
    uuid <- submission$`_uuid`
    attachments <- submission$`_attachments`
    audit_url <- NULL

    # Find the audit attachment
    if (!is.null(attachments) && length(attachments) > 0) {
      for (att in attachments) {
        filename <- att$filename %||% ""
        if (grepl("audit", filename, ignore.case = TRUE)) {
          audit_url <- att$download_url %||% att$url
          break
        }
      }
    }

    list(uuid = uuid, audit_url = audit_url)
  })

  # Filter to only submissions with audit URLs
  audit_info <- Filter(function(x) !is.null(x$audit_url), audit_info)

  if (length(audit_info) == 0) {
    cat(crayon::yellow(
      "No audit files found. Make sure audit logging is enabled in the Kobo form.\n"
    ))
    return(invisible(audit_dir))
  }

  # Skip UUIDs that are already downloaded
  already_downloaded <- list.dirs(
    audit_dir,
    full.names = FALSE,
    recursive = FALSE
  )
  audit_info <- Filter(
    function(x) !x$uuid %in% already_downloaded,
    audit_info
  )

  if (length(audit_info) == 0) {
    cat(crayon::green("All audit files are already downloaded.\n"))
  } else {
    cat(crayon::yellow(paste0(
      "--> Downloading ",
      length(audit_info),
      " audit file(s)...\n"
    )))

    success_count <- 0
    fail_count <- 0
    total <- length(audit_info)

    for (i in seq_along(audit_info)) {
      uuid_i <- audit_info[[i]]$uuid
      url_i <- audit_info[[i]]$audit_url

      cat(crayon::yellow(paste0(
        "  [", i, "/", total, "] ", uuid_i, " ... "
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

          # Validate that we got actual audit data
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


#' Download KoBo Submission Media
#'
#' Downloads all image and media attachments for each submission from the KoBo server.
#' Files are automatically renamed based on their XLSForm question/indicator name
#' (e.g., photo_trash.jpg) instead of keeping generic timestamps. Folders are structured
#' by enumerator and submission UUID (enum_uuid) to maintain data tracking.
#' Fetches data directly from the KoBo JSON API and skips already-downloaded files automatically.
#'
#' @param asset_id String. The KoBo asset/form unique identifier (UID).
#' @param server_url String. The base URL of your KoBo server instance. Default is "https://kobo.impact-initiatives.org".
#' @param media_dir String. The target base directory path to save downloaded images. Default is "images/".
#' @param enum_col String. The exact field/column name in the form tracking the enumerator's identity. Default is "enum".
#' @param zip_output Logical. If TRUE, bundles all downloaded subfolders into a single zip archive. Default is TRUE.
#'
#' @return Invisibly returns the base media directory path string.
#' @export
kobo_download_media <- function(
  asset_id,
  server_url = "https://kobo.impact-initiatives.org",
  media_dir = "images/",
  enum_col = "enum",
  zip_output = TRUE
) {
  # Retrieve API authentication token
  token <- kobo_get_token()

  # Ensure the base output folder exists
  if (!dir.exists(media_dir)) {
    dir.create(media_dir, recursive = TRUE)
    cat(crayon::yellow(paste0("Created media directory: ", media_dir, "\n")))
  }

  # Set up initial data tracking URL
  cat(crayon::yellow("--> Fetching submission data from Kobo API...\n"))
  data_url <- paste0(server_url, "/api/v2/assets/", asset_id, "/data.json")

  all_results <- list()
  next_url <- data_url

  # Loop and paginate through all available submission pages
  while (!is.null(next_url)) {
    req <- httr::GET(
      next_url,
      httr::add_headers(Authorization = paste("Token", token))
    )

    if (httr::status_code(req) != 200) {
      stop("Failed to fetch submission data. Status code: ", httr::status_code(req))
    }

    res <- httr::content(req, "parsed")
    all_results <- c(all_results, res$results)
    next_url <- res$`next` # Move to next page if available
  }

  cat(crayon::green(paste0("Found ", length(all_results), " submission(s).\n")))

  if (length(all_results) == 0) {
    cat(crayon::yellow("No submissions found.\n"))
    return(invisible(media_dir))
  }

  cat(crayon::yellow("--> Processing and downloading media named by indicators...\n"))

  success_count <- 0
  fail_count <- 0
  skipped_count <- 0

  # Iterate through each submission record individually
  for (submission in all_results) {
    uuid <- submission$`_uuid`
    enum_name <- submission[[enum_col]] %||% "unknown_enum"
    attachments <- submission$`_attachments`

    # Skip if submission has no files attached
    if (is.null(attachments) || length(attachments) == 0) next

    # Build trackable unique directory name (enum_uuid)
    folder_name <- paste0(enum_name, "_", uuid)
    uuid_dir <- file.path(media_dir, folder_name)

    # Flatten the payload structure to map nested group questions easily
    flat_submission <- unlist(submission)
    fields <- names(flat_submission)

    # Evaluate individual attachments within the submission
    for (att in attachments) {
      raw_filename <- att$filename %||% ""

      # Exclude blank entries or text audit logs
      if (is.null(raw_filename) || is.na(raw_filename) || raw_filename == "" ||
        grepl("audit", raw_filename, ignore.case = TRUE)) {
        next
      }

      indicator_name <- NULL

      # Strategy 1: Match raw filename string to its specific XLSForm key
      match_idx <- which(sapply(flat_submission, function(val) {
        if (!is.character(val) || is.na(val) || val == "") {
          return(FALSE)
        }
        identical(val, raw_filename) || identical(basename(val), basename(raw_filename))
      }))

      if (length(match_idx) > 0) {
        indicator_name <- fields[match_idx[1]]
        indicator_name <- stringr::str_replace(indicator_name, ".*/", "") # Strip group path prefix
      }

      # Strategy 2: Fallback to attachment block properties (name/instance_id)
      if (is.null(indicator_name) || is.na(indicator_name) || indicator_name == "") {
        potential_name <- att$name %||% att$instance_id
        if (!is.null(potential_name) && !is.na(potential_name) && potential_name != "") {
          indicator_name <- stringr::str_replace(potential_name, ".*/", "")
        }
      }

      # Strategy 3: Ultimate fallback to raw file stem if all matching fails
      if (is.null(indicator_name) || is.na(indicator_name) || indicator_name == "") {
        indicator_name <- tools::file_path_sans_ext(basename(raw_filename))
      }

      # Validate and extract file extensions safely
      file_ext <- tools::file_ext(raw_filename)
      if (is.null(file_ext) || is.na(file_ext) || file_ext == "") file_ext <- "jpg"

      # Define ultimate destination file path
      dest_file <- file.path(uuid_dir, paste0(indicator_name, ".", file_ext))

      # Skip execution if file is already stored locally
      if (file.exists(dest_file)) {
        skipped_count <- skipped_count + 1
        next
      }

      download_url <- att$download_url %||% att$url
      if (is.null(download_url) || is.na(download_url)) next

      # Create subfolder on-demand once a download is confirmed
      if (!dir.exists(uuid_dir)) {
        dir.create(uuid_dir, showWarnings = FALSE, recursive = TRUE)
      }

      cat(crayon::yellow(paste0("  Downloading: ", folder_name, " -> ", indicator_name, " ... ")))

      # Execute authenticated stream download
      tryCatch(
        {
          media_req <- httr::GET(
            download_url,
            httr::write_disk(dest_file, overwrite = TRUE),
            httr::add_headers(Authorization = paste("Token", token)),
            httr::timeout(120)
          )

          if (httr::status_code(media_req) == 200) {
            cat(crayon::green("OK\n"))
            success_count <- success_count + 1
          } else {
            cat(crayon::red(paste0("HTTP ", httr::status_code(media_req), "\n")))
            fail_count <- fail_count + 1
            if (file.exists(dest_file)) file.remove(dest_file) # Remove empty artifacts
          }
        },
        error = function(e) {
          cat(crayon::red(paste0("Error: ", e$message, "\n")))
          fail_count <- fail_count + 1
          if (file.exists(dest_file)) file.remove(dest_file)
        }
      )
    }
  }

  # Print processing summary logs
  cat(crayon::green(paste0("\nDone! ", success_count, " files downloaded")))
  if (skipped_count > 0) cat(crayon::blue(paste0(", ", skipped_count, " already existed")))
  if (fail_count > 0) cat(crayon::red(paste0(", ", fail_count, " failed")))
  cat("\n")

  # Archive processing results if requested
  if (zip_output) {
    media_folders <- list.dirs(media_dir, full.names = TRUE, recursive = FALSE)

    if (length(media_folders) > 0) {
      zip_path <- paste0(
        tools::file_path_sans_ext(sub("/$", "", media_dir)),
        ".zip"
      )

      cat(crayon::yellow(paste0("--> Zipping media files to: ", zip_path, "\n")))

      # Package subfolders cleanly using cherry-pick mode
      zip::zip(
        zipfile = zip_path,
        files = media_folders,
        mode = "cherry-pick"
      )
      cat(crayon::green("Zip file created successfully.\n"))
    } else {
      cat(crayon::yellow("No media folders found to zip.\n"))
    }
  }

  return(invisible(media_dir))
}
