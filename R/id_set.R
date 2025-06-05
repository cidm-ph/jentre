#' Entrez identifier lists
#' 
#' Many Entrez APIs accept either a UID list or tokens that point to a result stored
#' on its history server. The S3 classes here wrap these and keep track of the
#' database name that the identifiers belong to. Most of the API helpers in this
#' package are generic over the type of ID set and so can be used the same way with
#' either type. For large numbers of UIDs, the web history versions are generally
#' recommended to avoid running into server-side request timeouts.
#' 
#' `entrez_id_list` can be sliced and indexed like a vector (e.g. `id_set[1:10]` or
#' `tail(id_set)`).
#' 
#' `entrez_web_history` is an opaque reference to an object stored on the Entrez
#' history server. Unless the length is known at the time the object is
#' constructed, it will be missing.
#' 
#' Convert `entrez_id_list` to `entrez_web_history` with [`epost()`].
#' 
#' Convert `entrez_web_history` to `entrez_id_list` with [`entrez_translate()`].
#' 
#' @rdname entrez_id_set
#' @param db name of the associated Entrez database (e.g. `"biosample"`).
#' @param ids UIDs, coercible to a character vector (can be accessions or GI numbers).
#' @export
entrez_id_list <- function(db, ids) {
  structure(
    list(database = db, ids = as.character(ids)),
    class = c("entrez_id_list", "entrez_id_set")
  )
}
# TODO use vctrs to make these proper vector classes

#' @rdname entrez_id_set
#' @param query_key,WebEnv history server tokens returned by another Entrez API call
#' @param length number of UIDs in the set, if known
#' @export
entrez_web_history <- function(db, query_key, WebEnv, length = NA) {
  structure(
    list(database = db, query_key = query_key, WebEnv = WebEnv, length = length),
    class = c("entrez_web_history", "entrez_id_set")
  )
}

#' @rdname entrez_id_set
#' @param x object to test.
#' @export
is.entrez_id_set <- function(x) inherits(x, "entrez_id_set")

#' @rdname entrez_id_set
#' @export
is.entrez_id_list <- function(x) inherits(x, "entrez_id_list")

#' @rdname entrez_id_set
#' @export
is.entrez_web_history <- function(x) inherits(x, "entrez_web_history")

#' @rdname entrez_id_set
#' @param id_set ID set object.
#' @export
entrez_ids <- function(id_set) UseMethod("entrez_ids")

#' @rdname entrez_id_set
#' @export
entrez_database <- function(id_set) UseMethod("entrez_database")

#' @export
length.entrez_id_list <- function(x) length(x$ids)

#' @export
mtfrm.entrez_id_list <- function(x) x$ids

#' @export
format.entrez_id_list <- function(x, ...) {
  paste0("<entrez/", x$database, " [", length(x$ids), " UIDs]>")
}
#' @export
print.entrez_id_list <- function(x, ...) {
  print(format(x, ...))
}

#' @export
format.entrez_web_history <- function(x, ...) {
  len <- x$length
  if (is.na(len)) len <- "?"
  wenv <- x$WebEnv
  wenv <- substr(wenv, nchar(wenv) - 4, nchar(wenv))
  paste0("<entrez@/", x$database, " ", wenv, "#", x$query_key, " [", len, " UIDs]>")
}
#' @export
print.entrez_web_history <- function(x, ...) {
  print(format(x, ...))
}

#' @export
entrez_ids.entrez_id_list <- function(id_set) id_set$ids

#' @export
entrez_database.entrez_id_list <- function(id_set) id_set$database

#' @export
`[.entrez_id_list` <- function(x, i, ...) {
  entrez_id_list(x$database, x$ids[i, ...])
}

#' @export
`[[.entrez_id_list` <- function(x, i, ...) {
  entrez_id_list(x$database, x$ids[[i, ...]])
}

#' @export
entrez_database.entrez_web_history <- function(id_set) id_set$database

#' @export
length.entrez_web_history <- function(x) x$length

#' Check ID set is well formed
#' 
#' @param id_set ID set object.
#' @param database name of intended database.
#'   If `NULL` the database name is not checked.
#' @return `id_set`. This function raises an error if any check fails.
#' @export
check_id_set <- function(id_set, database = NULL, .call = rlang::caller_env()) {
  if (!is.null(database)) {
    actual_db <- entrez_database(id_set) 
    if (database != actual_db) {
      cli::cli_abort(c(
        "ID set provided belongs to a different Entrez database",
        "x" = "ID set {.emph {format(id_set)}} is not from database {.field {database}}"
      ), call = .call)
    }
  }

  if (is.entrez_id_list(id_set)) {
    if (any(is.na(id_set$ids))) stop("NA IDs in id_set")
  }

  id_set
}

compute_n_items <- function(id_set, call = rlang::caller_env()) {
  n_items <- length(id_set)
  if (is.entrez_web_history(id_set) && is.na(n_items)) {
    n_items <- entrez_count(id_set, .call = call)
  }
  n_items
}