# Set Kobo API Token

Securely saves the Kobo API token using keyring. If a token is not
provided, the user will be prompted to enter it interactively.

## Usage

``` r
kobo_setup_token(token = NULL)
```

## Arguments

- token:

  Character. The Kobo API token. If NULL (default), the function prompts
  for the token interactively.
