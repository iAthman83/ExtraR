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
  tryCatch({
    keyring::key_get("kobo_api_token")
  }, error = function(e) {
    stop("Kobo API token not found. Please run kobo_setup_token() first.")
  })
}

#' Trigger Kobo Export
#'
#' @param asset_id The Kobo asset ID
#' @param server_url The Kobo server URL
#' @return The export URL to poll
#' @export
kobo_export_trigger <- function(asset_id, server_url = "https://kobo.impact-initiatives.org") {
  token <- kobo_get_token()
  
  export_url <- paste0(server_url, "/api/v2/assets/", asset_id, "/exports/")
  
  payload <- list(
    type = "xls",
    lang = "_xml",
    fields_from_all_versions = TRUE,
    multiple_select = "both"
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
    stop("Failed to trigger export. Status code: ", httr::status_code(req), " Response: ", httr::content(req, "text"))
  }
}

#' Poll Kobo Export Status
#'
#' @param export_url The export URL returned by `kobo_export_trigger`
#' @param sleep_time Seconds to wait between polling
#' @return The download URL when successful
#' @export
kobo_export_poll <- function(export_url, sleep_time = 5) {
  token <- kobo_get_token()
  
  cat(crayon::yellow("Polling export status...\n"))
  repeat {
    req <- httr::GET(
      export_url,
      httr::add_headers(Authorization = paste("Token", token))
    )
    
    if (httr::status_code(req) == 200) {
      res <- httr::content(req, "parsed")
      status <- res$job_status
      
      if (status == "SUCCESS") {
        cat(crayon::green("Export successful.\n"))
        return(res$result)
      } else if (status == "FAILED") {
        stop("Export failed on Kobo server.")
      }
      
      Sys.sleep(sleep_time)
    } else {
      stop("Failed to poll export status. Status code: ", httr::status_code(req))
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
kobo_download_data <- function(asset_id, server_url = "https://kobo.impact-initiatives.org", cache_dir = "data/") {
  # Ensure cache dir exists
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  
  cat(crayon::green(paste0("--> Triggering export for asset: ", asset_id, "\n")))
  export_url <- kobo_export_trigger(asset_id, server_url)
  
  download_url <- kobo_export_poll(export_url)
  
  # Ensure cache filename is unique per asset
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0("kobo_data_", asset_id, "_", timestamp, ".xlsx")
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
