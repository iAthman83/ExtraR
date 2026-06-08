# Download Kobo Audit Files

Downloads audit log files for each submission from the Kobo server.
Audit files track enumerator timing (how long each survey question
took). Fetches audit URLs directly from the Kobo JSON API, so it works
regardless of how the main data was downloaded. Already-downloaded
audits are skipped automatically.

## Usage

``` r
kobo_download_audits(
  asset_id,
  server_url = "https://kobo.impact-initiatives.org",
  audit_dir = "audits/",
  zip_output = TRUE
)
```

## Arguments

- asset_id:

  The Kobo asset ID.

- server_url:

  The Kobo server URL.

- audit_dir:

  Directory to save the downloaded audit files.

- zip_output:

  If TRUE, creates a zip file of all audit folders after downloading
  (default: TRUE).

## Value

Invisibly returns the audit directory path.
