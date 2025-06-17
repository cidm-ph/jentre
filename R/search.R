#' Search Entrez databases
#' 
#' The search term field names are documented in the EInfo API endpoint:
#' see [`einfo()`].
#' 
#' @family API methods
#' @param term search query.
#' @param db Entrez database name.
#' @param retstart integer: index of first result (starts from 0).
#'   Ignored when `usehistory` is `TRUE`.
#' @param retmax integer: maximum number of results to return.
#'   When `NA` this returns all results.
#'   When `NULL`, uses the Entrez default (typically 20).
#'   Note that it is possible that slightly more than `retmax` results will be returned
#'   when paginating.
#'   Ignored when `usehistory` is `TRUE`.
#' @param retmode character: currently only `"xml"` is supported.
#' @param rettype character: currently only `"uilist"` is supported.
#' @param usehistory logical: when `TRUE` use the history server to return the result.
#' @param WebEnv,query_key either characters to pass on as-is, or [`entrez_web_history`] objects.
#' @param .paginate controls how multiple API requests are used to complete the call.
#'   Pagination is performed using the `retstart` and `retmax` API parameters.
#'   When set to an integer, no more than `.pagniate` items will be requested per API call.
#'   When `FALSE` or `0`, only one API request is sent.
#'   Ignored when `usehistory` is `TRUE`.
#' @param .path path specification for saving raw responses.
#'   See `path` argument of [`httr2::req_perform_iterative()`].
#' @inheritParams entrez_request
#' @return id set object (either a [`web_history`] or an [`id_list`]).
#' @seealso <https://www.ncbi.nlm.nih.gov/books/NBK25499/#chapter4.ESearch>
#' @export
esearch <- function(
  term,
  db,
  ...,
  retstart = 0L,
  retmax = NA,
  retmode = "xml",
  rettype = "uilist",
  usehistory = is.null(retmax) || is.na(retmax),
  WebEnv = NULL,
  query_key = NULL,
  .cookies = NA,
  .paginate = 10000L,
  .progress = "ESearch",
  .path = NULL,
  .call = current_env()
) {
  force(usehistory)

  if (retmode != "xml") {
    cli::cli_abort("Currently only XML format is supported", call = .call)
  }
  if (rettype != "uilist") {
    cli::cli_abort("Currently only the uilist return type is supported", call = .call)
  }

  .paginate <- as.integer(.paginate)
  if (!is.null(retmax) && !is.na(retmax) && retmax <= .paginate) .paginate <- 0L

  params <- list2(
    db = db,
    term = term,
    retstart = retstart,
    retmode = retmode,
    retmax = retmax,
    rettype = rettype,
    usehistory = if (usehistory) "y" else NULL,
    !!!webhist_params(WebEnv = WebEnv, query_key = query_key),
    ...
  )

  if (usehistory) {
    params$retstart <- NULL
    params$retmax <- NULL
    req <- new_request("esearch.fcgi", params, .cookies = .cookies, .call = .call)
    httr2::req_perform(req, path = path_glue_dummy(.path)) |>
      parse_response("xml", call = .call) |>
      check_xml_eSearchResult() |>
      inform_xml_eSearchResult() |>
      process_response(fn = process_xml_eSearchResult_webhist(db), call = .call)
  } else if (.paginate == 0L) {
    if (is.na(retmax)) {
      cli::cli_abort("{.arg retmax} cannot be {.val NA} when pagination is disabled", call = .call)
    }
    req <- new_request("esearch.fcgi", params, .cookies = .cookies, .call = .call)
    ids <-
      httr2::req_perform(req, path = path_glue_dummy(.path)) |>
      parse_response("xml", call = .call) |>
      check_xml_eSearchResult() |>
      inform_xml_eSearchResult() |>
      process_response(fn = process_xml_eSearchResult_uilist, call = .call)
    id_list(db, ids)
  } else {
    if (is.na(.cookies)) {
      .cookies <- tempfile()
      on.exit(if (file.exists(.cookies)) file.remove(.cookies), add = TRUE)
    }

    params$retmax <- .paginate
    req <- new_request("esearch.fcgi", params, .cookies = .cookies, .call = .call)
    ids <-
      req |>
      httr2::req_perform_iterative(
        max_reqs = if (is.na(retmax)) Inf else ceiling((retmax - retstart) / .paginate),
        path = .path,
        on_error = "return",
        progress = .progress,
        next_req = httr2::iterate_with_offset(
          param_name = "retstart",
          start = retstart,
          offset = .paginate,
          resp_pages = function(resp) {
            n_items <- if (is.na(retmax)) {
              parse_response(resp, "xml", call = call) |>
                process_response(fn = process_xml_eSearchResult_count, call = .call)
            } else {
              retmax - retstart
            }
            ceiling(n_items / .paginate)
          }
        )
      ) |>
      httr2::resps_successes() |>
      httr2::resps_data(search_resps_data(.call))
    
    id_list(db, ids)
  }
}

search_resps_data <- function(.call) {
  env <- environment()
  function(resp) {
    doc <- parse_response(resp, "xml", call = .call) |>
      check_xml_eSearchResult()
    if (rlang::env_cache(env, ".first", TRUE)) {
      rlang::env_poke(env, ".first", FALSE)
      inform_xml_eSearchResult(doc)
    }
    process_response(doc, fn = process_xml_eSearchResult_uilist, call = .call)
  }
}

check_xml_eSearchResult <- function(doc) {
  check_xml_root(doc, "eSearchResult", call = caller_env())

  for (error in xml_find_all(doc, "//ErrorList/*")) {
    cli::cli_alert_danger("{.field eSearch} {.field {xml2::xml_name(error)}}: {xml_text(error)}", class = "esearch_error")
  }
  for (warning in xml_find_all(doc, "//WarningList/OutputMessage")) {
    cli::cli_alert_warning("{.field eSearch} {xml_text(warning)}", class = "esearch_warning")
  }

  invisible(doc)
}

inform_xml_eSearchResult <- function(doc) {
  count <- xml_find_first(doc, "/eSearchResult/Count") |> xml2::xml_integer()
  translation <- xml_find_first(doc, "//QueryTranslation") |> xml_text()
  cli::cli_alert_info("{.field eSearch} query {.val {translation}} has {.strong {count}} results", class = "esearch_info")

  invisible(doc)
}

process_xml_eSearchResult_webhist <- function(db) {
  function(doc) {
    web_history(
      db = db,
      query_key = doc |> xml_find_first("/eSearchResult/QueryKey") |> xml_text(),
      WebEnv = doc |> xml_find_first("/eSearchResult/WebEnv") |> xml_text(),
      length = doc |> xml_find_first("/eSearchResult/Count") |> xml2::xml_integer()
    )
  }
}

process_xml_eSearchResult_count <- function(doc) {
  doc |> 
    xml_find_first("/eSearchResult/Count") |>
    xml2::xml_integer()
}

process_xml_eSearchResult_uilist <- function(doc) {
  doc |>
    xml_find_all("/eSearchResult/IdList/Id") |>
    xml_text()
}