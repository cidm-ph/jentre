# Construct a request to the Entrez API

This is a low-level helper that builds a request object but does not
perform the request. In general you'll likely use higher-level methods
like [`efetch()`](https://cidm-ph.github.io/jentre/reference/efetch.md)
instead.

## Usage

``` r
entrez_request(
  endpoint,
  ...,
  .method = "GET",
  .multi = "comma",
  .cookies = NULL,
  .verbose = getOption("jentre.verbose", default = TRUE),
  .call = current_env()
)

entrez_api_key(default = NULL)
```

## Arguments

- endpoint:

  Entrez endpoint name (e.g. `"efetch.fcgi"`).

- ...:

  additional API parameters (refer to Entrez documentation). Any set to
  `NULL` are removed.

- .method:

  HTTP verb. For `"POST"`, any params with vector values (usually just
  `id`) are sent in the request body instead of the URL.

- .multi:

  controls how repeated params are handled (see
  [`httr2::req_url_query()`](https://httr2.r-lib.org/reference/req_url.html)).

- .cookies:

  path to persist cookies. If `NULL`, cookies are not added to the
  request. For helper functions: when `NA`, a temporary file is created
  (in this case only, the temporary file will be cleaned up once all
  requests are performed).

- .verbose:

  logical: when TRUE logs all API requests as messages in a compact
  format. This uses a summarised format that does not include the
  request body for POST. Use normal httr verbosity controls (e.g.
  [`httr2::local_verbosity()`](https://httr2.r-lib.org/reference/with_verbosity.html))
  to override this behaviour and see more details.

- .call:

  call environment to use in error messages/traces. See
  [rlang::topic-error-call](https://rlang.r-lib.org/reference/topic-error-call.html)
  and the `call` argument of
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html).
  You only need to specify this in internal helper functions that don't
  need to be mentioned in error messages.

- default:

  default value to return if no global configuration is found.

## Value

- for `entrez_request()` an
  [`httr2::request`](https://httr2.r-lib.org/reference/request.html)
  object.

- for `entrez_api_key()`, the API key as a character, or `default` if no
  global config exists.

## Details

`email`, `tool`, and `api_key` have default values but these can be
overridden, or can be removed by setting them to `NULL`.

## API limits

The Entrez APIs are rate limited. Requests in this package respect the
API headers returned by Entrez. Without an API key you will be rate
limited more aggressively, so it is recommended to [obtain an API
key](https://support.nlm.nih.gov/kbArticle/?pn=KA-05317). `jentre`
searches for the API key in the following order:

- the API parameter `entrez_key` provided to any API request function,

- the [option](https://rdrr.io/r/base/options.html) `"jentre.api_key"`,
  then

- the environment variable `ENTREZ_KEY`.

You can check the value is found properly using `entrez_api_key()`. If
no API key is set, a warning will be displayed. This can be suppressed
by setting the option `"jentre.silence_api_warning"` to `TRUE`.
