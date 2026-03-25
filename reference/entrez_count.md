# Count the number of entries in an ID set

If `id_set` is an
[`id_list`](https://cidm-ph.github.io/jentre/reference/id_set.md) then
this is equivalent to [`length()`](https://rdrr.io/r/base/length.html).
If it is a `web_history`, this may involve an Entrez API call to get the
number of entries. In this case the result is cached so that subsequent
calls don't hit the API again.

## Usage

``` r
entrez_count(id_set, .call = current_env())
```

## Arguments

- id_set:

  an ID set object.

- .call:

  call environment to use in error messages/traces. See
  [rlang::topic-error-call](https://rlang.r-lib.org/reference/topic-error-call.html)
  and the `call` argument of
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html).
  You only need to specify this in internal helper functions that don't
  need to be mentioned in error messages.

## Value

integer number of entries.

## Examples

``` r
id_set <- id_list("sra", c("39889350", "39889348", "39889347"))
entrez_count(id_set)
#> [1] 3
```
