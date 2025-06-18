#' @import vctrs
NULL

#' Entrez identifier sets
#' 
#' Many Entrez APIs accept either a UID list or tokens that point to a result stored
#' on its history server. The classes here wrap these and keep track of the
#' database name that the identifiers belong to. Most of the API helpers in this
#' package are generic over the type of ID set and so can be used the same way with
#' either type. For large numbers of UIDs, the web history versions are generally
#' recommended to avoid running into server-side request timeouts.
#' 
#' `id_list` is a vector and can be manipulated to take subsets (e.g. `id_set[1:10]` or
#' `tail(id_set)`).
#' 
#' `web_history` is an opaque reference to an ID list stored on the Entrez
#' history server. Through the course of API calls, information about the length or
#' the actual list of IDs may be discovered and cached, avoiding subsequent API calls.
#' `as_id_list()` can be used to extract the list of IDs.
#' 
#' Convert `id_list` to `web_history` with [`epost()`].
#' Convert `web_history` to `id_list` with `as_id_list()`.
#' 
#' @rdname id_set
#' @param db name of the associated Entrez database (e.g. `"biosample"`).
#' @param ids UIDs, coercible to a character vector (can be accessions or GI numbers).
#' @seealso [`entrez_validate()`] and [`entrez_count()`]
#' @returns
#'  * For `id_list()` and `as_id_list()` an `id_list` vector.
#'  * For `web_history()` a `web_history` object.
#'  * For `is_id_set()`, `is_id_list()`, and `is_web_history()` a logical.
#' @export
id_list <- function(db, ids = character()) {
  check_scalar_character(db, allow_na = FALSE)
  check_character(ids, allow_na = FALSE)
  new_id_list(db, ids)
}
new_id_list <- function(db, ids = character()) {
  vctrs::new_vctr(
    ids,
    database = db,
    class = c("jentre_id_list", "jentre_id_set")
  )
}

#' @rdname id_set
#' @param query_key,WebEnv history server tokens returned by another Entrez API call.
#' @param length number of UIDs in the set, if known.
#' @export
web_history <- function(db, WebEnv, query_key, length = NA) {
  check_scalar_character(db, allow_na = FALSE)
  check_scalar_character(WebEnv, allow_na = FALSE)
  check_scalar_character(query_key, allow_na = FALSE)
  length <- vctrs::vec_cast(length, integer())
  check_scalar_integer(length, allow_na = TRUE)

  query_key <- as.integer(query_key)
  x <- vctrs::vec_recycle_common(WebEnv, query_key, length)
  vctrs::list_check_all_size(x, 1)
  new_web_history(db, x[[1]], x[[2]], x[[3]])
}
new_web_history <- function(db, WebEnv, query_key, length = NA) {
  x <- vctrs::new_rcrd(
    list(
      WebEnv = WebEnv,
      query_key = query_key
    ),
    database = db,
    data = new.env(parent = emptyenv()),
    class = c("jentre_web_history", "jentre_id_set")
  )

  if (!is.na(length)) wh_len_set(x, length)

  x
}

#' @rdname id_set
#' @param x object to test or convert.
#' @export
is_id_set <- function(x) inherits(x, "jentre_id_set")
#' @rdname id_set
#' @export
is_id_list <- function(x) inherits(x, "jentre_id_list")
#' @rdname id_set
#' @export
is_web_history <- function(x) inherits(x, "jentre_web_history")
#' @export

vec_ptype_full.jentre_id_list <- function(x, ...) paste0("entrez/", attr(x, "database"))
#' @export
vec_ptype_abbr.jentre_id_list <- function(x, ...) "idlst"
#' @export
format.jentre_id_list <- function(x, ...) {
  format(vctrs::vec_data(x))
}

il_ids_get <- function(x) vctrs::vec_data(x)

#' @export
vec_ptype_full.jentre_web_history <- function(x, ...) paste0("entrez@/", attr(x, "database"))
#' @export
vec_ptype_abbr.jentre_web_history <- function(x, ...) "wbhst"
#' @export
format.jentre_web_history <- function(x, ...) {
  env <- abbreviate(vctrs::field(x, "WebEnv"), method = "both.sides", minlength = 10, use.classes = FALSE, dot = TRUE)
  keys <- vctrs::field(x, "query_key")
  lens <- wh_len_get(x)
  lens[is.na(lens)] <- "?"
  format(paste0(env, "#", keys, "[", lens, "]"))
}

wh_webenv <- function(x) vctrs::field(x, "WebEnv")
wh_qrykey <- function(x) vctrs::field(x, "query_key")
wh_ids_set <- function(x, ids) {
  stopifnot(is_web_history(x))
  rlang::env_poke(attr(x, "data"), "len", length(ids))
  rlang::env_poke(attr(x, "data"), "ids", ids)
}
wh_ids_get <- function(x) {
  rlang::env_cache(attr(x, "data"), "ids", NULL)
}
wh_len_set <- function(x, length) {
  stopifnot(is_web_history(x))
  rlang::env_poke(attr(x, "data"), "len", length)
}
wh_len_get <- function(x) {
  rlang::env_cache(attr(x, "data"), "len", NA_integer_)
}

#' @export
vec_ptype2.jentre_id_list.jentre_id_list <- function(x, y, ...) {
  check_compatible_db(x, y, call = caller_env())
  new_id_list(entrez_database(x))
}
#' @export
vec_ptype2.jentre_web_history.jentre_web_history <- function(x, y, ...) {
  check_compatible_db(x, y, call = caller_env())
  new_web_history(entrez_database(x), NA_character_, NA_character_)
}
#' @export
vec_ptype2.jentre_id_list.character <- function(x, y, ...) character()
#' @export
vec_ptype2.character.jentre_id_list <- function(x, y, ...) character()
#' @export
vec_cast.jentre_id_list.character <- function(x, to, ...) id_list(attr(to, "database"), x)
#' @export
vec_cast.character.jentre_id_list <- function(x, to, ...) vctrs::vec_data(x)

#' @export
vec_ptype2.jentre_id_list.jentre_web_history <- function(x, y, ...) {
  check_compatible_db(x, y, call = caller_env())
  new_id_list(entrez_database(x))
}
#' @export
vec_ptype2.jentre_web_history.jentre_id_list <- vec_ptype2.jentre_id_list.jentre_web_history
#' @export
vec_cast.jentre_id_list.jentre_web_history <- function(x, to, ...) {
  check_compatible_db(x, to, call = caller_env())
  ids <- wh_ids_get(x)
  if (is.null(ids)) {
    stop_incompatible_cast(
      x, to,
      x_arg = rlang::caller_arg(x),
      to_arg = rlang::caller_arg(to),
      message = "IDs are not cached; use as_id_list() instead"
    )
  }
  new_id_list(attr(to, "database"), ids)
}

check_compatible_db <- function(
  x,
  y,
  ...,
  x_arg = rlang::caller_arg(x),
  y_arg = rlang::caller_arg(y)
) {
  if (entrez_database(x) != entrez_database(y)) {
    vctrs::stop_incompatible_type(x, y, ..., x_arg = x_arg, y_arg = y_arg)
  }
}

#' Check ID set is well formed
#' 
#' @param x ID set object.
#' @param database name of intended database.
#'   If `NULL` the database name is not checked.
#' @param call execution environment, for error reporting.
#'   See [rlang::topic-error-call] and the `call` argument of [cli::cli_abort()].
#' @param arg name of argument to use in error reporting.
#' @return
#'  * For `check_*`, these function raise an error if the check fails.
#'  * For `entrez_database()` the name of the database.
#' @export
check_id_set <- function(
  x,
  database = NULL,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is_id_set(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be an id set, not {.obj_type_friendly {x}}.",
      call = call
    )
  }

  if (!is.null(database)) {
    actual_db <- entrez_database(x) 
    if (database != actual_db) {
      cli::cli_abort(c(
        "{.arg {arg}} is an ID set from the wrong Entrez database",
        "x" = "ID set {.emph {format(id_set)}} is not from database {.field {database}}"
      ), call = call)
    }
  }
}
#' @rdname check_id_set
#' @export
check_id_list <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is_id_list(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be an id list, not {.obj_type_friendly {x}}.",
      call = call
    )
  }
}
#' @rdname check_id_set
#' @export
check_web_history <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is_web_history(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a web history, not {.obj_type_friendly {x}}.",
      call = call
    )
  }
}
#' @rdname check_id_set
#' @export
entrez_database <- function(x) {
  check_id_set(x)
  attr(x, "database")
}