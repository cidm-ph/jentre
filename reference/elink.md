# ELink API for fetching links between databases

`elink()` offers direct access to the ELink API endpoint, which has many
different input and output formats depending on parameters. If you just
want a one-to-one mapping of `neighbor` links, use `elink_map()`, which
handles this for you.

## Usage

``` r
elink(
  id_set,
  db,
  ...,
  retmode = "xml",
  cmd = NA,
  .paginate = 100L,
  .process = NA,
  .method = NA,
  .multi = "explode",
  .progress = TRUE,
  .cookies = NA,
  .path = NULL,
  .call = current_env()
)

elink_map(id_set, db, ..., .cookies = NA, .path = NULL, .call = current_env())
```

## Arguments

- id_set:

  ID set object.

- db:

  target database name.

- ...:

  additional API parameters (refer to Entrez documentation). Any set to
  `NULL` are removed.

- retmode:

  response format.

- cmd:

  ELink command. If `NA` either `"neighbor"` or `"neighbor_history"`
  will be used based on the type of input.

- .paginate:

  maximum number of UIDs to submit per request. `.paginate` only applies
  when `id_set` is an explicit list. Set to `FALSE` to prevent batching.

- .process:

  function that processes the API results. Can be a function or builtin
  processor as described in
  [`process`](https://cidm-ph.github.io/jentre/reference/topic-process.md).
  Additional builtin processors are available: `"sets"` to get a data
  frame of ID set objects, `"flat"` to get a data frame of UIDs, or `NA`
  to use a sensible choice based on parameters.

- .method:

  HTTP verb. For `"POST"`, any params with vector values (usually just
  `id`) are sent in the request body instead of the URL.

- .multi:

  controls how repeated params are handled (see
  [`httr2::req_url_query()`](https://httr2.r-lib.org/reference/req_url.html)).

- .progress:

  controls progress bar; see the `progress` argument of
  [`httr2::req_perform_iterative()`](https://httr2.r-lib.org/reference/req_perform_iterative.html).

- .cookies:

  path to persist cookies. If `NULL`, cookies are not added to the
  request. For helper functions: when `NA`, a temporary file is created
  (in this case only, the temporary file will be cleaned up once all
  requests are performed).

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

concatenated output of `.process`. For `elink(.process = "sets")` a data
frame with columns

- `from`:

  Source link set.

- `to`:

  Target link set.

- `linkname`:

  Link name (see
  <https://eutils.ncbi.nlm.nih.gov/entrez/query/static/entrezlinks.html>
  or [`einfo`](https://cidm-ph.github.io/jentre/reference/einfo.md)).

For `elink_map()` and `elink(.process = "flat")` a data frame with
columns

- `db_from`:

  Source database name.

- `id_from`:

  Source identifier. Can be a list column depending on how `elink` was
  called.

- `db_to`:

  Target database name.

- `linkname`:

  Link name (see
  <https://eutils.ncbi.nlm.nih.gov/entrez/query/static/entrezlinks.html>
  or [`einfo`](https://cidm-ph.github.io/jentre/reference/einfo.md)).

- `id_to`:

  Target identifier. In general this will be a list column.

## One-to-one mapping

Note that some ways of calling this API on multiple UIDs result in the
one-to-one association of the input and output sets getting lost. The
way around this is to specify each ID as a separate parameter rather
than a single comma-separated param. This is handled by the default
choice of `.multi = "explode"`. When using a web history token as input,
there is no corresponding way to ensure one-to-one mapping. To ensure
that the result is always one-to-one, use `elink_map()`, which may make
several API requests to achieve the result.

## See also

Other API methods:
[`efetch()`](https://cidm-ph.github.io/jentre/reference/efetch.md),
[`einfo()`](https://cidm-ph.github.io/jentre/reference/einfo.md),
[`entrez_validate()`](https://cidm-ph.github.io/jentre/reference/entrez_validate.md),
[`epost()`](https://cidm-ph.github.io/jentre/reference/epost.md),
[`esearch()`](https://cidm-ph.github.io/jentre/reference/esearch.md),
[`esummary()`](https://cidm-ph.github.io/jentre/reference/esummary.md)
