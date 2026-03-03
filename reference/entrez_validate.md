# Look up accessions and other IDs on Entrez

Passes the provided IDs through Entrez which has the effect of
normalising the accepted UIDs, and removing invalid UIDs. For web
history lists, this forces results to be freshly downloaded (unlike
[`as_id_list()`](https://cidm-ph.github.io/jentre/reference/id_set.md)
which can use cached results).

## Usage

``` r
entrez_validate(id_set, .paginate = 5000L, .path = NULL, .call = current_env())
```

## Arguments

- id_set:

  an [`id_list`](https://cidm-ph.github.io/jentre/reference/id_set.md)
  object.

- .paginate:

  controls how multiple API requests are used to complete the call.
  Pagination is performed using the `retstart` and `retmax` API
  parameters. When set to an integer, no more than `.pagniate` items
  will be requested per API call. When `FALSE` or `0`, only one API
  request is sent.

- .path:

  path specification for saving raw responses.

- .call:

  call environment to use in error messages/traces. See
  [rlang::topic-error-call](https://rlang.r-lib.org/reference/topic-error-call.html)
  and the `call` argument of
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html).
  You only need to specify this in internal helper functions that don't
  need to be mentioned in error messages.

## Value

[`id_list`](https://cidm-ph.github.io/jentre/reference/id_set.md) object

## See also

Other API methods:
[`efetch()`](https://cidm-ph.github.io/jentre/reference/efetch.md),
[`einfo()`](https://cidm-ph.github.io/jentre/reference/einfo.md),
[`elink()`](https://cidm-ph.github.io/jentre/reference/elink.md),
[`epost()`](https://cidm-ph.github.io/jentre/reference/epost.md),
[`esearch()`](https://cidm-ph.github.io/jentre/reference/esearch.md),
[`esummary()`](https://cidm-ph.github.io/jentre/reference/esummary.md)
