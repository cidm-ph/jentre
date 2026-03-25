# Register UIDs with the Entrez history server

Register UIDs with the Entrez history server

## Usage

``` r
epost(id_set, ..., WebEnv = NULL, .path = NULL, .call = rlang::current_env())
```

## Arguments

- id_set:

  an [`id_list`](https://cidm-ph.github.io/jentre/reference/id_set.md)
  object.

- ...:

  additional API parameters (refer to Entrez documentation). Any set to
  `NULL` are removed.

- WebEnv:

  either a character to pass on as-is, or a
  [`web_history`](https://cidm-ph.github.io/jentre/reference/id_set.md)
  object.

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

A [`web_history`](https://cidm-ph.github.io/jentre/reference/id_set.md)
object usable with other API functions.

## See also

Other API methods:
[`efetch()`](https://cidm-ph.github.io/jentre/reference/efetch.md),
[`einfo()`](https://cidm-ph.github.io/jentre/reference/einfo.md),
[`elink()`](https://cidm-ph.github.io/jentre/reference/elink.md),
[`entrez_validate()`](https://cidm-ph.github.io/jentre/reference/entrez_validate.md),
[`esearch()`](https://cidm-ph.github.io/jentre/reference/esearch.md),
[`esummary()`](https://cidm-ph.github.io/jentre/reference/esummary.md)

## Examples
