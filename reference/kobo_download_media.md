# Download KoBo Submission Media

Downloads all image and media attachments for each submission from the
KoBo server. Files are automatically renamed based on their XLSForm
question/indicator name (e.g., photo_trash.jpg) instead of keeping
generic timestamps. Folders are structured by enumerator and submission
UUID (enum_uuid) to maintain data tracking. Fetches data directly from
the KoBo JSON API and skips already-downloaded files automatically.

## Usage

``` r
kobo_download_media(
  asset_id,
  server_url = "https://kobo.impact-initiatives.org",
  media_dir = "images/",
  enum_col = "enum",
  zip_output = TRUE
)
```

## Arguments

- asset_id:

  String. The KoBo asset/form unique identifier (UID).

- server_url:

  String. The base URL of your KoBo server instance. Default is
  "https://kobo.impact-initiatives.org".

- media_dir:

  String. The target base directory path to save downloaded images.
  Default is "images/".

- enum_col:

  String. The exact field/column name in the form tracking the
  enumerator's identity. Default is "enum".

- zip_output:

  Logical. If TRUE, bundles all downloaded subfolders into a single zip
  archive. Default is TRUE.

## Value

Invisibly returns the base media directory path string.
