# Get details about Entrez databases

These functions call the EInfo endpoint. `einfo()` provides the number
of entries in the databases, the name and description, list of terms
usable in the query syntax, and list of link names usable with the ELink
endpoint.

## Usage

``` r
einfo(db, ..., retmode = "xml", version = "2.0", .call = rlang::current_env())

einfo_databases(..., retmode = "xml", .call = rlang::current_env())
```

## Arguments

- db:

  name of database to provide information about.

- ...:

  additional API parameters (refer to Entrez documentation). Any set to
  `NULL` are removed.

- retmode:

  response format.

- version:

  response format version.

- .call:

  call environment to use in error messages/traces. See
  [rlang::topic-error-call](https://rlang.r-lib.org/reference/topic-error-call.html)
  and the `call` argument of
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html).
  You only need to specify this in internal helper functions that don't
  need to be mentioned in error messages.

## Value

Character vector of database names for `einfo_databases()`. An XML
document with root node `<eInfoResult>` for `einfo()`.

## See also

Other API methods:
[`efetch()`](https://cidm-ph.github.io/jentre/reference/efetch.md),
[`elink()`](https://cidm-ph.github.io/jentre/reference/elink.md),
[`entrez_validate()`](https://cidm-ph.github.io/jentre/reference/entrez_validate.md),
[`epost()`](https://cidm-ph.github.io/jentre/reference/epost.md),
[`esearch()`](https://cidm-ph.github.io/jentre/reference/esearch.md),
[`esummary()`](https://cidm-ph.github.io/jentre/reference/esummary.md)

## Examples
