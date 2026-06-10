# Download Kobo Data

Main function to download data from Kobo.

## Usage

``` r
kobo_download_data(
  asset_id,
  server_url = "https://kobo.impact-initiatives.org",
  cache_dir = "data/",
  filename = paste0("kobo_data_", asset_id, ".xlsx")
)
```

## Arguments

- asset_id:

  The Kobo asset ID

- server_url:

  The Kobo server URL (default: "https://kobo.impact-initiatives.org")

- cache_dir:

  Directory to save the downloaded file

- filename:

  The name of the downloaded file (default:
  "kobo_data\_\<asset_id\>.xlsx")

## Value

The path to the downloaded Excel file
