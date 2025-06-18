#' Fetch records from Entrez
#' 
#' Fetching can be slow, and Entrez will time out requests that take too long.
#' This helper supports pagination if you specify `retmax`.
#' 
#' @family API methods
#' @param id_set ID set object.
#' @param retstart integer: index of first result (starts from 0).
#' @param retmax integer: maximum number of results to return.
#'  When `NA` this returns all results. When `NULL`, uses the Entrez default (typically 20).
#'  Note that when using pagination with web history, it is possible that slightly more than
#'  `retmax` results will be returned.
#' @param retmode character: requested document file format.
#' @param rettype character: requested document type.
#' @param .method HTTP verb. If `NA`, a sensible default is chosen based on the request parameters.
#' @param .process function that processes the API results.
#'  Can be a function or builtin processor as described in [`process`].
#'  Additional builtin processors are available:
#'    * `"uilist"` to extract a list of IDs (suitable for `rettype = "uilist"`),
#'    * `NA` to use a sensible choice based on parameters. In particular, for `"uilist"`
#'      requests, it will return an [`id_list`] object.
#' @param .paginate controls how multiple API requests are used to complete the call.
#'   Pagination is performed using the `retstart` and `retmax` API parameters.
#'   When set to an integer, no more than `.pagniate` items will be requested per API call.
#'   When `FALSE` or `0`, only one API request is sent.
#' @param .progress controls progress bar; see the `progress` argument of
#'   [`httr2::req_perform_iterative()`].
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
  rettype = NULL,
  .method = NA,
  .cookies = NA,
  .paginate = 200L,
  .process = NA,
  .progress = "Fetching",
  .path = NULL,
  .call = rlang::current_env()
) {
  efetch_impl(
    id_set,
    ...,
    retstart = retstart,
    retmax = retmax,
    retmode = retmode,
    rettype = rettype,
    .method = .method,
    .cookies = .cookies,
    .process = .process,
    .paginate = .paginate,
    .call = .call,
    .endpoint = "efetch.fcgi",
    .progress = .progress,
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
  .progress = "Fetching summaries",
  .path = NULL,
  .call = rlang::current_env()
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
    .progress = .progress,
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
  rettype = NULL,
  .method = NA,
  .cookies = NA,
  .process = process_identity,
  .paginate = 20L,
  .call = rlang::caller_env(),
  .endpoint = "efetch.fcgi",
  .progress = "Fetching",
  .path = NULL,
  .first_index = 0L
) {
  # convert to ID list if possible without hitting the API
  id_set <- prefer_id_list(id_set)

  if (is.na(.method)) {
    .method <- if (is_web_history(id_set)) "GET" else "POST"
  }

  .paginate <- as.integer(.paginate)
  n_items <- NA
  if (is.na(retmax)) {
    n_items <- entrez_count(id_set)
    retmax <- n_items - retstart + .first_index
  } else {
    n_items <- retmax + retstart - .first_index
  }
  if (n_items <= .paginate) .paginate <- 0L

  # subset to the requested range locally, if we can
  if (is_id_list(id_set) && length(id_set) > 1L) {
    idx_start <- retstart + 1L - .first_index
    idx_end <- min(idx_start + retmax - 1L, length(id_set))
    id_set <- id_set[idx_start:idx_end]
    stopifnot(length(id_set) == n_items)
  }

  if (identical(rettype, "uilist") && rlang::is_na(.process)) {
    .process <- process_xml_eFetchResult_uilist_with_db(entrez_database(id_set))
  }
  if (rlang::is_na(.process)) .process <- process_identity

  params <- rlang::list2(
    !!!entrez_id_params(id_set),
    retstart = retstart,
    retmax = retmax,
    retmode = retmode,
    ...
  )

  if (.paginate == 0L) {
    req <- new_request(.endpoint, params, .method = .method, .cookies = .cookies, .call = .call)
    resp <-
      httr2::req_perform(req, path = path_glue_dummy(.path)) |>
      parse_response(retmode, call = .call) |>
      process_response(fn = .process, envir = .process_efetch, call = .call, arg = ".process")
    return(resp)
  }

  # we'll need multiple requests, so make a cookie file now if requested
  if (is.na(.cookies)) {
    .cookies <- tempfile()
    on.exit(if (file.exists(.cookies)) file.remove(.cookies), add = TRUE)
  }

  req <- NA
  n_batches <- NA

  next_req <- if (is_id_list(id_set)) {
    sets <- split_id_list(id_set, max_per_batch = .paginate)
    n_batches <- length(sets)
    params$id <- il_ids_get(sets[[1]])
    params$retstart <- NULL
    params$retmax <- .paginate
    req <- new_request(
      .endpoint, params, .method = "POST", .body_params = c("id"),
      .cookies = .cookies, .call = .call
    )
    iterate_body_form(id = Map(il_ids_get, sets[2:length(sets)]), .multi = "comma", .call = .call)
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
      progress = .progress
    ) |>
    httr2::resps_successes() |>
    httr2::resps_data(function(resp) {
      parse_response(resp, retmode, call = .call) |>
        process_response(fn = .process, envir = .process_efetch, call = .call, arg = ".process")
    })
}

process_xml_eFetchResult_uilist_with_db <- function(db) {
  function(doc) {
    ids <- process_xml_eFetchResult_uilist(doc)
    id_list(db, ids)
  }
}

process_xml_eFetchResult_uilist <- function(doc) {
  check_xml_root(doc, "IdList")
  doc |> xml_find_all("/IdList/Id") |> xml_text()
}

#' @include process.R
.process_efetch <- new.env(parent = .process_common)
.process_efetch$uilist <- process_xml_eFetchResult_uilist