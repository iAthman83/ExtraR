# Poll Kobo Export Status

Poll Kobo Export Status

## Usage

``` r
kobo_export_poll(export_url, sleep_time = 5, max_retries = 60)
```

## Arguments

- export_url:

  The export URL returned by `kobo_export_trigger`

- sleep_time:

  Seconds to wait between polling

- max_retries:

  Maximum number of retries before timing out

## Value

The download URL when successful
