#' Fetch records from Entrez
#' 
#' Fetching can be slow, and Entrez will time out requests that take too long.
#' This helper supports pagination if you specify `retmax`.
#' 
#' @family API methods
#' @param id_set ID set object.
#' @param retstart integer: index of first result (starts from 0).
#' @param retmax integer: maximum number of results to return.
#' @param retmode character: currently only `"xml"` is supported.
#' @param .method HTTP verb. If `NA`, a sensible default is chosen based on the request parameters.
#' @param .process function that processes the API results.
#'   Can be a function or builtin processor as described in [`process`].
#' @param .paginate logical: when `TRUE` send as many queries as needed to get the complete result.
#' @inheritParams entrez_request
#' @return output of `.process` from each page of results, combined with [`vctrs::list_unchop()`].
#' @export
efetch <- function(
  id_set,
  ...,
  retstart = 0L,
  retmax = 500L,
  retmode = "xml",
  .method = NA,
  .cookies = NA,
  .paginate = TRUE,
  .process = "identity",
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
    .endpoint = "efetch.fcgi"
  )
}

#' Fetch document summaries from Entrez
#' 
#' ESummary is faster than EFetch because it only interacts with the frontend
#' rather than the full database. It contains more limited information.
#' Consider adding `version = "2.0"` to request the revised output format.
#' 
#' @family API methods
#' @param retstart integer: index of first result (starts from 1).
#'   Note: this differes from other endpoints that start from 0.
#' @inheritParams efetch
#' @export
esummary <- function(
  id_set,
  ...,
  retstart = 1L,
  retmax = 5000L,
  retmode = "xml",
  version = "2.0",
  .method = NA,
  .cookies = NA,
  .paginate = NA,
  .process = "identity",
  .call = rlang::caller_env()
) {
  if (is.na(.paginate)) .paginate <- is.entrez_web_history(id_set)

  if (is.entrez_id_list(id_set) && (.paginate || retstart > 1L)) {
    # confirmed this with Entrez support
    cli::cli_abort(c(
      "The ESummary API only supports pagination with {.param retstart} when using the history server",
      "i" = "First submit your list of UIDs with {.fn epost}"
    ), call = .call)
  }

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
    .endpoint = "esummary.fcgi"
  )
}

# Allows efetch and esummary to share an implementation since they mostly
# differ by the endpoint name (and output format, of course).
efetch_impl <- function(
  id_set,
  ...,
  retstart = 0L,
  retmax = 500L,
  retmode = "xml",
  .method = NA,
  .cookies = NA,
  .process = process_identity,
  .paginate = TRUE,
  .call = rlang::caller_env(),
  .endpoint = "efetch.fcgi"
) {
  if (is.na(.cookies)) {
    .cookies <- tempfile()
    on.exit(if (file.exists(.cookies)) file.remove(.cookies), add = TRUE)
  }

  if (is.na(.method)) {
    .method <- if (is.entrez_web_history(id_set)) "GET" else "POST"
  }

  .process <- as_function(.process, env = .process_common, call = .call)

  params <- rlang::list2(
    !!!entrez_id_params(id_set),
    retmax = retmax,
    retmode = retmode,
    ...
  )
  req <- new_request(.endpoint, params, .method = .method, .cookies = .cookies, .call = .call)

  if (.paginate) {
    efetch_paginate(req, retstart, retmax, retmode, compute_n_items(id_set, call = .call), .process, call = .call)
  } else {
    efetch_direct(req, retmode, .process, call = .call)
  }
}

efetch_direct <- function(req, retmode, .process, call) {
  req |>
    httr2::req_perform() |>
    parse_response(retmode, call = call) |>
    .process()
}

efetch_paginate <- function(req, retstart, retmax, retmode, n_items, .process, call) {
  req |>
    httr2::req_perform_iterative(
      max_reqs = Inf,
      on_error = "return",
      progress = "Fetching",
      next_req = httr2::iterate_with_offset(
        param_name = "retstart",
        start = retstart,
        offset = retmax,
        resp_pages = function(resp) ceiling(n_items / retmax)
      )
    ) |>
    httr2::resps_successes() |>
    httr2::resps_data(function(resp) {
      parse_response(resp, retmode, call = call) |> .process()
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