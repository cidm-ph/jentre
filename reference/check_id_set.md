# Check ID set is well formed

Check ID set is well formed

## Usage

``` r
check_id_set(
  x,
  database = NULL,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
)

check_id_list(x, arg = rlang::caller_arg(x), call = rlang::caller_env())

check_web_history(x, arg = rlang::caller_arg(x), call = rlang::caller_env())

entrez_database(x)
```

## Arguments

- x:

  ID set object.

- database:

  name of intended database. If `NULL` the database name is not checked.

- arg:

  name of argument to use in error reporting.

- call:

  execution environment, for error reporting. See
  [rlang::topic-error-call](https://rlang.r-lib.org/reference/topic-error-call.html)
  and the `call` argument of
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html).

## Value

- For `check_*`, these function raise an error if the check fails.

- For `entrez_database()` the name of the database.
