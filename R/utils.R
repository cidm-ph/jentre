#' @importFrom rlang !!! list2 current_env caller_env
NULL

parse_response <- function(resp, retmode, ..., errors = TRUE, call = rlang::caller_env()) {
  # TODO handle application errors
  doc <- switch(
    retmode,
    xml = httr2::resp_body_xml(resp, ...),
    json = httr2::resp_body_json(resp, ...),
    httr2::resp_body_string(resp, ...)
  ) 
  if (errors && retmode == "xml") raise_xml_error(doc, call = call)
  doc
}

raise_xml_error <- function(doc, call = rlang::caller_env()) {
  err <- xml_find_first(doc, "//ERROR")
  if (!is.na(err)) {
    cli::cli_abort(
      c("Entrez error encountered in XML response", "x" = xml_text(err)),
      call = call
    )
  }
  doc
}

entrez_id_params <- function(id_set, call = rlang::caller_env()) {
  check_id_set(id_set, call = call)
  if (is_id_list(id_set)) {
    list(db = entrez_database(id_set), id = il_ids_get(id_set))
  } else if (is_web_history(id_set)) {
    list(db = entrez_database(id_set), query_key = wh_qrykey(id_set), WebEnv = wh_webenv(id_set))
  } else {
    stop(paste0("Unimplemented for ", class(id_set)))
  }
}

webhist_params <- function(WebEnv = NULL, query_key = NULL, call = rlang::caller_env()) {
  if (!is.null(WebEnv) && is_web_history(WebEnv)) WebEnv <- wh_webenv(WebEnv)
  if (!is.null(query_key) && is_web_history(query_key)) query_key <- wh_qrykey(query_key)

  list(WebEnv = WebEnv, query_key = query_key)
}

split_id_list <- function(id_set, max_per_batch) {
  n_batches <- ceiling(length(id_set) / max_per_batch)
  n_per_batch <- ceiling(length(id_set) / n_batches)
  starts <- seq(from = 1, to = length(id_set) - 1, by = n_per_batch)
  ends <- unique(c(seq(from = n_per_batch, to = length(id_set), by = n_per_batch), length(id_set)))
  purrr::map2(starts, ends, \(x, y) id_set[x:y])
}

req_body_form_modify <- function(req, ..., .multi = "error", .call = rlang::caller_env()) {
  data <- rlang::list2(...)
  if (is.null(req$body) || req$body$type != "form") {
    cli::cli_abort("Can only be used after {.fn httr2::req_body_form}", call = .call)
  }
  new_body <- utils::modifyList(req$body$data, data)
  httr2::req_body_form(req, !!!new_body, .multi = .multi)
}

# Helper for use with httr2::req_perform_iterative().
#
# @param ... parameters to iterate over. Each must be a
#   list of the same length. Each request will take the
#   first value of each such list and use it to update
#   the request body.
iterate_body_form <- function(..., .multi = "error", .call = rlang::caller_env()) {
  params <- rlang::list2(...)
  stopifnot(length(params) > 0, rlang::is_named2(params))
  lengths <- sapply(params, length)
  stopifnot(all(lengths == lengths[[1]]))
  batch_env <- environment()
  
  function(resp, req) {
    values <- Map(\(x) unlist(utils::head(x, n = 1)), params)
    if (sapply(values, length)[[1]] == 0) return(NULL)
    remainder <- Map(\(x) utils::tail(x, n = -1), params)
    assign("params", remainder, envir = batch_env)
    req_body_form_modify(req, !!!values, .multi = .multi, .call = .call)
  }
}

# for using with httr::req_perform*(path) in cases where
# we need to account for single or multi request branches
# with the same glue spec
path_glue_dummy <- function(path) {
  if (is.null(path)) {
    NULL
  } else {
    glue::glue(path, .envir = emptyenv(), i = 1L)
  }
}

check_arg <- function(test, what) {
  function(
    x,
    ...,
    allow_na = TRUE,
    allow_null = FALSE,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()
  ) {
    if (!missing(x)) {
      if (test(x)) {
        if (!allow_na && any(is.na(x))) {
          cli::cli_abort("{.arg {arg}} can't contain NA values.", call = call)
        }
        return(invisible(NULL))
      }
      if (allow_null && rlang::is_null(x)) {
        return(invisible(NULL))
      }
    }

    what <- list(what)
    if (allow_na) what[length(what) + 1] <- "`NA`"
    if (allow_null) what[length(what) + 1] <- "`NULL`"
    cli::cli_abort("{.arg {arg}} must be {.or {what}}, not {.obj_type_friendly {x}}.", ..., call = call)
  }
}

check_character <- check_arg(rlang::is_character, "a character vector")
check_scalar_character <- check_arg(rlang::is_scalar_character, "a character scalar")
check_scalar_integer <- check_arg(rlang::is_scalar_integer, "a scalar integer")