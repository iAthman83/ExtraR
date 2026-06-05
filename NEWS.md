# extrar (development version)

## Infrastructure Updates

* Added unit testing framework using `testthat` (edition 3).
* Enabled Markdown parsing in function documentation (`roxygen2`).
* Configured permissive MIT License.
* Added README badges for build status (`R-CMD-check`).

## Core Features

### Data Preparation & Kobo Integration
* Added `kobo_download_data()` to download survey datasets securely from KoboToolbox via its API.
* Added `read_raw_data()` and `read_loop_data()` to standardize data types and manage composite loop keys.

### "Other" responses
* Centralized and mapped "other" text inputs using `prepare_other_responses()`.
* Generated styled Excel files for translation/recoding with `save_other_responses()`.

### Survey Analysis Pipeline
* Implemented `run_group_analysis_pipeline()` to automate survey analysis across multiple grouping variables.
* Added `format_my_xlsx_variable_x_group()` to export fully styled, group-aligned Excel sheets.
