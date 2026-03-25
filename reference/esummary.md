# Fetch document summaries from Entrez

ESummary is faster than EFetch because it only interacts with the
frontend rather than the full database. It contains more limited
information.

## Usage

``` r
esummary(
  id_set,
  ...,
  retstart = 0L,
  retmax = NA,
  retmode = "xml",
  version = "2.0",
  .method = NA,
  .cookies = NA,
  .paginate = 5000L,
  .process = "identity",
  .progress = "Fetching summaries",
  .path = NULL,
  .call = rlang::current_env()
)
```

## Arguments

- id_set:

  ID set object.

- ...:

  additional API parameters (refer to Entrez documentation). Any set to
  `NULL` are removed.

- retstart:

  integer: index of first result (starts from 0).

- retmax:

  integer: maximum number of results to return. When `NA` this returns
  all results. When `NULL`, uses the Entrez default (typically 20). Note
  that when using pagination with web history, it is possible that
  slightly more than `retmax` results will be returned.

- retmode:

  character: requested document file format.

- version:

  character: requested format version.

- .method:

  HTTP verb. If `NA`, a sensible default is chosen based on the request
  parameters.

- .cookies:

  path to persist cookies. If `NULL`, cookies are not added to the
  request. For helper functions: when `NA`, a temporary file is created
  (in this case only, the temporary file will be cleaned up once all
  requests are performed).

- .paginate:

  controls how multiple API requests are used to complete the call.
  Pagination is performed using the `retstart` and `retmax` API
  parameters. When set to an integer, no more than `.pagniate` items
  will be requested per API call. When `FALSE` or `0`, only one API
  request is sent.

- .process:

  function that processes the API results. Can be a function or builtin
  processor as described in
  [`process`](https://cidm-ph.github.io/jentre/reference/topic-process.md).
  Additional builtin processors are available:

  - `"uilist"` to extract a list of IDs (suitable for
    `rettype = "uilist"`),

  - `NA` to use a sensible choice based on parameters. In particular,
    for `"uilist"` requests, it will return an
    [`id_list`](https://cidm-ph.github.io/jentre/reference/id_set.md)
    object.

- .progress:

  controls progress bar; see the `progress` argument of
  [`httr2::req_perform_iterative()`](https://httr2.r-lib.org/reference/req_perform_iterative.html).

- .path:

  path specification for saving raw responses. See `path` argument of
  [`httr2::req_perform_iterative()`](https://httr2.r-lib.org/reference/req_perform_iterative.html).

- .call:

  call environment to use in error messages/traces. See
  [rlang::topic-error-call](https://rlang.r-lib.org/reference/topic-error-call.html)
  and the `call` argument of
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html).
  You only need to specify this in internal helper functions that don't
  need to be mentioned in error messages.

## Value

Combined output of `.process` from each page of results. For the default
where `.process` does nothing, this will be a list of XML documents. For
other choices, it can be a vector, list, or data frame.

## See also

Other API methods:
[`efetch()`](https://cidm-ph.github.io/jentre/reference/efetch.md),
[`einfo()`](https://cidm-ph.github.io/jentre/reference/einfo.md),
[`elink()`](https://cidm-ph.github.io/jentre/reference/elink.md),
[`entrez_validate()`](https://cidm-ph.github.io/jentre/reference/entrez_validate.md),
[`epost()`](https://cidm-ph.github.io/jentre/reference/epost.md),
[`esearch()`](https://cidm-ph.github.io/jentre/reference/esearch.md)
