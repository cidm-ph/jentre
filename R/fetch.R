#' Fetch records from Entrez
#' 
#' Fetching can be slow, and Entrez will time out requests that take too long.
#' This helper supports pagination if you specify `retmax`.
#' 
#' @family API methods
#' @param id_set ID set object.
#' @param retstart integer: index of first result (starts from 0).
#' @param retmax integer: maximum number of results to return.
#'   When `NA` this returns all results. When `NULL`, uses the Entrez default (typically 20).
#'   Note that when using pagination with web history, it is possible that slightly more than
#'   `retmax` results will be returned.
#' @param retmode character: requested document format.
#' @param .method HTTP verb. If `NA`, a sensible default is chosen based on the request parameters.
#' @param .process function that processes the API results.
#'   Can be a function or builtin processor as described in [`process`].
#' @param .paginate controls how multiple API requests are used to complete the call.
#'   Pagination is performed using the `retstart` and `retmax` API parameters.
#'   When set to an integer, no more than `.pagniate` items will be requested per API call.
#'   When `FALSE` or `0`, only one API request is sent.
#' @param .path path specification for saving raw responses.
#'   See `path` argument of [`httr2::req_perform_iterative()`].
#' @inheritParams entrez_request
#' @return output of `.process` from each page of results, combined with [`vctrs::list_unchop()`].
#' @export
efetch <- function(
  id_set,
  ...,
  retstart = 0L,
  retmax = NA,
  retmode = "xml",
  .method = NA,
  .cookies = NA,
  .paginate = 200L,
  .process = "identity",
  .path = NULL,
  .call = rlang::caller_env()
) {
  efetch_impl(
    id_set,
    ...,
    retstart = retstart,
    retmax = retmax,
    retmode = retmode,
    .method = .method,
    .cookies = .cookies,
    .process = .process,
    .paginate = .paginate,
    .call = .call,
    .endpoint = "efetch.fcgi",
    .path = .path,
    .first_index = 0L,
  )
}

#' Fetch document summaries from Entrez
#' 
#' ESummary is faster than EFetch because it only interacts with the frontend
#' rather than the full database. It contains more limited information.
#' Consider adding `version = "2.0"` to request the revised output format.
#' 
#' @family API methods
#' @param version character: requested format version.
#' @inheritParams efetch
#' @export
esummary <- function(
  id_set,
  ...,
  retstart = 0L,
  retmax = NA,
  retmode = "xml",
  version = "2.0",
  .method = NA,
  .cookies = NA,
  .paginate = 5000L,
  .process = "identity",
  .path = NULL,
  .call = rlang::caller_env()
) {
  efetch_impl(
    id_set,
    ...,
    retstart = retstart,
    retmax = retmax,
    retmode = retmode,
    version = version,
    .method = .method,
    .cookies = .cookies,
    .process = .process,
    .paginate = .paginate,
    .call = .call,
    .endpoint = "esummary.fcgi",
    .path = .path,
    .first_index = 0L, # documented as starting at 1 but this appears to be wrong
  )
}

# Allows efetch and esummary to share an implementation since they mostly
# differ by the endpoint name (and output format, of course).
efetch_impl <- function(
  id_set,
  ...,
  retstart = 0L,
  retmax = NA,
  retmode = "xml",
  .method = NA,
  .cookies = NA,
  .process = process_identity,
  .paginate = 20L,
  .call = rlang::caller_env(),
  .endpoint = "efetch.fcgi",
  .path = NULL,
  .first_index = 0L
) {
  if (is.na(.method)) {
    .method <- if (is.entrez_web_history(id_set)) "GET" else "POST"
  }

  .process <- as_function(.process, env = .process_common, call = .call)
  .paginate <- as.integer(.paginate)

  n_items <- NA
  if (is.na(retmax)) {
    n_items <- compute_n_items(id_set)
    retmax <- n_items - retstart + .first_index
  } else {
    n_items <- retmax - retstart + .first_index
  }
  if (n_items <= .paginate) .paginate <- 0L

  # subset to the requested range locally, if we can
  if (is.entrez_id_list(id_set)) {
    idx_start <- retstart + 1L - .first_index
    idx_end <- idx_start + retmax - 1L
    id_set <- id_set[idx_start:idx_end]
    stopifnot(length(id_set) == n_items)
  }

  params <- rlang::list2(
    !!!entrez_id_params(id_set),
    retstart = retstart,
    retmax = retmax,
    retmode = retmode,
    ...
  )

  if (.paginate == 0L) {
    get_path <- function(i) {
      if (is.null(path)) NULL
      else {
        glue_env <- new.env(parent = emptyenv())
        glue_env$i <- 1L
        glue::glue(path, .envir = glue_env)
      }
    }
    req <- new_request(.endpoint, params, .method = .method, .cookies = .cookies, .call = .call)
    resp <-
      httr2::req_perform(req, path = get_path(.path)) |>
      parse_response(retmode, call = .call) |>
      .process()
    return(resp)
  }

  # we'll need multiple requests, so make a cookie file now if requested
  if (is.na(.cookies)) {
    .cookies <- tempfile()
    on.exit(if (file.exists(.cookies)) file.remove(.cookies), add = TRUE)
  }

  req <- NA
  n_batches <- NA

  next_req <- if (is.entrez_id_list(id_set)) {
    sets <- split_id_list(id_set, max_per_batch = .paginate)
    n_batches <- length(sets)
    params$id <- entrez_ids(sets[[1]])
    params$retstart <- NULL
    params$retmax <- .paginate
    req <- new_request(.endpoint, params, .method = .method, .cookies = .cookies, .call = .call)
    iterate_body_form(id = Map(entrez_ids, sets[2:length(sets)]), .multi = "comma", .call = .call)
  } else {
    n_batches <- ceiling(n_items / .paginate)
    n_per_batch <- ceiling(n_items / n_batches)
    params$retmax <- n_per_batch
    req <- new_request(.endpoint, params, .method = .method, .cookies = .cookies, .call = .call)
    httr2::iterate_with_offset(
      param_name = "retstart",
      start = retstart,
      offset = n_per_batch,
      resp_pages = function(resp) n_batches
    )
  }

  req |>
    httr2::req_perform_iterative(
      next_req = next_req,
      path = .path,
      max_reqs = n_batches,
      on_error = "return",
      progress = "Fetching"
    ) |>
    httr2::resps_successes() |>
    httr2::resps_data(function(resp) {
      parse_response(resp, retmode, call = .call) |> .process()
    })
}

#' Convert web history result or accessions and other IDs to Entrez UIDs
#' 
#' @family API methods
#' @inheritParams efetch
#' @export
entrez_translate <- function(id_set, retmax = 10000L, .call = rlang::caller_env()) {
  .method <- if (is.entrez_id_list(id_set)) "POST" else "GET"

  params <- rlang::list2(
    !!!entrez_id_params(id_set),
    retmax = retmax,
    rettype = "uilist",
    retmode = "xml",
  )
  req <- new_request("efetch.fcgi", params, .method = .method)

  ids <- efetch_paginate(
    req,
    retstart = 0L,
    retmax = retmax,
    retmode = "xml",
    n_items = compute_n_items(id_set, call = .call),
    .process = function(doc) { doc |> xml_find_all("/IdList/Id") |> xml_text() },
    call = .call
  )

  entrez_id_list(db = entrez_database(id_set), ids = ids)
}