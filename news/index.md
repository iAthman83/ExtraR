# Changelog

## extrar 2026.6.5

### Infrastructure Updates

- Added unit testing framework using `testthat` (edition 3).
- Enabled Markdown parsing in function documentation (`roxygen2`).
- Configured permissive MIT License.
- Added README badges for build status (`R-check.yaml`).

### Core Features

#### Data Preparation & Kobo Integration

- Added
  [`kobo_download_data()`](https://iathman83.github.io/ExtraR/reference/kobo_download_data.md)
  to download survey datasets securely from KoboToolbox via its API.
- Added
  [`read_raw_data()`](https://iathman83.github.io/ExtraR/reference/read_raw_data.md)
  and
  [`read_loop_data()`](https://iathman83.github.io/ExtraR/reference/read_loop_data.md)
  to standardize data types and manage composite loop keys.

#### “Other” responses

- Centralized and mapped “other” text inputs using
  [`prepare_other_responses()`](https://iathman83.github.io/ExtraR/reference/prepare_other_responses.md).
- Generated styled Excel files for translation/recoding with
  [`save_other_responses()`](https://iathman83.github.io/ExtraR/reference/save_other_responses.md).

#### Survey Analysis Pipeline

- Implemented
  [`run_group_analysis_pipeline()`](https://iathman83.github.io/ExtraR/reference/run_group_analysis_pipeline.md)
  to automate survey analysis across multiple grouping variables.
- Added
  [`format_my_xlsx_variable_x_group()`](https://iathman83.github.io/ExtraR/reference/format_my_xlsx_variable_x_group.md)
  to export fully styled, group-aligned Excel sheets.
