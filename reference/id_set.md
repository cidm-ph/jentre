# Entrez identifier sets

Many Entrez APIs accept either a UID list or tokens that point to a
result stored on its history server. The classes here wrap these and
keep track of the database name that the identifiers belong to. Most of
the API helpers in this package are generic over the type of ID set and
so can be used the same way with either type.

## Usage

``` r
id_list(db, ids = character())

web_history(db, WebEnv, query_key, length = NA)

is_id_set(x)

is_id_list(x)

is_web_history(x)

as_id_list(x, .paginate = 5000L, .path = NULL, .call = current_env())
```

## Arguments

- db:

  name of the associated Entrez database (e.g. `"biosample"`).

- ids:

  UIDs, coercible to a character vector (can be accessions or GI
  numbers).

- query_key, WebEnv:

  history server tokens returned by another Entrez API call.

- length:

  number of UIDs in the set, if known.

- x:

  object to test or convert.

- .paginate:

  controls how multiple API requests are used to complete the call.
  Pagination is performed using the `retstart` and `retmax` API
  parameters. When set to an integer, no more than `.pagniate` items
  will be requested per API call. When `FALSE` or `0`, only one API
  request is sent. Ignored when `usehistory` is `TRUE`.

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

- For `id_list()` and `as_id_list()` an `id_list` vector.

- For `web_history()` a `web_history` object.

- For `is_id_set()`, `is_id_list()`, and `is_web_history()` a logical.

## Details

It usually will not make sense to create `web_history()` objects
directly - they are short-lived pointers to results on the Entrez
history server and are created by other API calls.

`id_list` is a vector and can be manipulated to take subsets (e.g.
`id_set[1:10]` or `tail(id_set)`).

`web_history` is an opaque reference to an ID list stored on the Entrez
history server. Through the course of API calls, information about the
length or the actual list of IDs may be discovered and cached, avoiding
subsequent API calls. `as_id_list()` can be used to extract the list of
IDs.

Convert `id_list` to `web_history` with
[`epost()`](https://cidm-ph.github.io/jentre/reference/epost.md).
Convert `web_history` to `id_list` with `as_id_list()`.

## See also

[`entrez_validate()`](https://cidm-ph.github.io/jentre/reference/entrez_validate.md)
and
[`entrez_count()`](https://cidm-ph.github.io/jentre/reference/entrez_count.md)

## Examples

``` r
bioprojects <- id_list("bioproject", c("1241475"))
```
