# Search Entrez databases

The search term field names are documented in the EInfo API endpoint:
see [`einfo()`](https://cidm-ph.github.io/jentre/reference/einfo.md).

## Usage

``` r
esearch(
  term,
  db,
  ...,
  retstart = 0L,
  retmax = NA,
  retmode = "xml",
  rettype = "uilist",
  usehistory = is.null(retmax) || is.na(retmax),
  WebEnv = NULL,
  query_key = NULL,
  .cookies = NA,
  .paginate = 10000L,
  .progress = "ESearch",
  .path = NULL,
  .verbose = getOption("jentre.verbose", default = TRUE),
  .call = current_env()
)
```

## Arguments

- term:

  search query.

- db:

  Entrez database name.

- ...:

  additional API parameters (refer to Entrez documentation). Any set to
  `NULL` are removed.

- retstart:

  integer: index of first result (starts from 0). Ignored when
  `usehistory` is `TRUE`.

- retmax:

  integer: maximum number of results to return. When `NA` this returns
  all results. When `NULL`, uses the Entrez default (typically 20). Note
  that it is possible that slightly more than `retmax` results will be
  returned when paginating. Ignored when `usehistory` is `TRUE`.

- retmode:

  character: currently only `"xml"` is supported.

- rettype:

  character: currently only `"uilist"` is supported.

- usehistory:

  logical: when `TRUE` use the history server to return the result.

- WebEnv, query_key:

  either characters to pass on as-is, or
  [`web_history`](https://cidm-ph.github.io/jentre/reference/id_set.md)
  objects.

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
  request is sent. Ignored when `usehistory` is `TRUE`.

- .progress:

  controls progress bar; see the `progress` argument of
  [`httr2::req_perform_iterative()`](https://httr2.r-lib.org/reference/req_perform_iterative.html).

- .path:

  path specification for saving raw responses. See `path` argument of
  [`httr2::req_perform_iterative()`](https://httr2.r-lib.org/reference/req_perform_iterative.html).

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

## Value

An id set object (either a
[`web_history`](https://cidm-ph.github.io/jentre/reference/id_set.md) or
an [`id_list`](https://cidm-ph.github.io/jentre/reference/id_set.md)).

## See also

<https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch>

Other API methods:
[`efetch()`](https://cidm-ph.github.io/jentre/reference/efetch.md),
[`einfo()`](https://cidm-ph.github.io/jentre/reference/einfo.md),
[`elink()`](https://cidm-ph.github.io/jentre/reference/elink.md),
[`entrez_validate()`](https://cidm-ph.github.io/jentre/reference/entrez_validate.md),
[`epost()`](https://cidm-ph.github.io/jentre/reference/epost.md),
[`esummary()`](https://cidm-ph.github.io/jentre/reference/esummary.md)

## Examples

``` r
if (FALSE) { # \dontrun{
esearch("mpox virus[orgn]", "biosample")
# → esearch db="biosample" term="mpox virus[orgn]" retmode="xml" rettype="uilist" usehistory="y"
# ℹ eSearch query "\"Monkeypox virus\"[Organism]" has 7189 results
# <entrez@/biosample[1]>
# [1] MCID_69c36.#1[7189]
} # }
```
