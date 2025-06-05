#' Search Entrez databases
#' 
#' The search term field names are documented in the EInfo API endpoint:
#' see [`einfo()`].
#' 
#' @family API methods
#' @param term search query.
#' @param db Entrez database name.
#' @param retstart integer: index of first result (starts from 0).
#' @param retmax integer: maximum number of results to return.
#'   Ignored when `usehistory` is `TRUE`.
#' @param retmode character: currently only `"xml"` is supported.
#' @param usehistory logical: when `TRUE` use the history server to return the result.
#' @param WebEnv,query_key either characters to pass on as-is, or [`entrez_web_history`] objects.
#' @param .paginate logical: when `TRUE` send as many queries as needed to get the complete result.
#'   Ignored when `usehistory` is `TRUE`.
#' @inheritParams entrez_request
#' @return id set object (either a web history token or explicit list).
#' @seealso <https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch>
#' @export
esearch <- function(
  term,
  db,
  ...,
  retstart = 0L,
  retmax = 1000L,
  retmode = "xml",
  usehistory = TRUE,
  WebEnv = NULL,
  query_key = NULL,
  .cookies = NA,
  .paginate = TRUE,
  .call = rlang::caller_env()
) {
  if (is.na(.cookies)) {
    .cookies <- tempfile()
    on.exit(if (file.exists(.cookies)) file.remove(.cookies), add = TRUE)
  }

  if (!is.null(WebEnv) && !usehistory) {
    cli::cli_warn(c(
      "Entrez requires {.param usehistory} when using {.param WebEnv}",
      "i" = "Add {.code usehistory = TRUE} if this request does not work"
    ))
  }

  if (!is.null(WebEnv) && is.entrez_web_history(WebEnv)) WebEnv <- WebEnv$WebEnv
  if (!is.null(query_key) && is.entrez_web_history(query_key)) query_key <- query_key$query_key

  if (retmode != "xml") {
    cli::cli_abort("Currently only XML format is supported", call = .call)
  }

  params <- rlang::list2(
    db = db,
    term = term,
    retstart = retstart,
    retmode = retmode,
    retmax = if (!usehistory) retmax else NULL,
    usehistory = if (usehistory) "y" else NULL,
    WebEnv = WebEnv,
    query_key = query_key,
    ...
  )

  req <- new_request("esearch.fcgi", params, .cookies = .cookies, .call = .call)

  if (usehistory) {
    esearch_history(req, db, call = .call)
  } else if (!.paginate) {
    esearch_direct(req, db, call = .call)
  } else {
    esearch_direct_paginate(req, db, retstart, retmax, call = .call)
  }
}

#' Count entries on the history server without returning them
#'
#' @family API methods
#' @param id_set an `entrez_web_history` object.
#' @return integer number of entries.
#' @inheritParams esearch
#' @export
entrez_count <- function(id_set, .call = rlang::caller_env()) {
  stopifnot(is.entrez_web_history(id_set))

  params <- rlang::list2(
    db = entrez_database(id_set),
    term = "",
    retmode = "xml",
    rettype = "count",
    WebEnv = id_set$WebEnv,
    query_key = id_set$query_key
  )
  req <- new_request("esearch.fcgi", params, .call = .call)

  req |>
    httr2::req_perform() |>
    parse_response("xml", call = .call) |>
    xml_find_first("/eSearchResult/Count") |>
    xml_text() |>
    as.integer()
}

esearch_history <- function(req, db, call) {
  doc <- httr2::req_perform(req) |> parse_response("xml", call = call)
  
  entrez_web_history(
    db = db,
    query_key = doc |> xml_find_first("/eSearchResult/QueryKey") |> xml_text(),
    WebEnv = doc |> xml_find_first("/eSearchResult/WebEnv") |> xml_text(),
    length = doc |> xml_find_first("/eSearchResult/Count") |> xml_text() |> as.integer()
  )
}

esearch_direct <- function(req, db, call) {
  ids <-
    httr2::req_perform(req) |>
    parse_response("xml", call = call) |>
    process_xml_eSearchResult()
  
  entrez_id_list(db, ids)
}

esearch_direct_paginate <- function(req, db, retstart, retmax, call) {
  retmax <- as.integer(retmax)

  ids <-
    req |>
    httr2::req_perform_iterative(
      max_reqs = Inf,
      on_error = "return",
      progress = "ESearch",
      next_req = httr2::iterate_with_offset(
        param_name = "retstart",
        start = retstart,
        offset = retmax,
        resp_pages = function(resp) {
          n_items <-
            parse_response(resp, "xml", call = call) |>
            xml_find_first("/eSearchResult/Count") |>
            xml_text() |>
            as.integer()
          ceiling(n_items / retmax)
        }
      )
    ) |>
    httr2::resps_successes() |>
    httr2::resps_data(function(resp) {
      parse_response(resp, "xml", call = rlang::caller_env()) |> process_xml_eSearchResult()
    })
  
  entrez_id_list(db, ids)
}

process_xml_eSearchResult <- function(doc) {
  doc |> xml_find_all("/eSearchResult/IdList/Id") |> xml_text()
}