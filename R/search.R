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
#' @return id set object (either a web history token or explicit list).
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
  .path = NULL,
  .call = rlang::current_env()
) {
  force(usehistory)

  if (is.na(.cookies)) {
    .cookies <- tempfile()
    on.exit(if (file.exists(.cookies)) file.remove(.cookies), add = TRUE)
  }

  if (!is.null(WebEnv) && is.entrez_web_history(WebEnv)) WebEnv <- WebEnv$WebEnv
  if (!is.null(query_key) && is.entrez_web_history(query_key)) query_key <- query_key$query_key

  if (retmode != "xml") {
    cli::cli_abort("Currently only XML format is supported", call = .call)
  }
  if (rettype != "uilist") {
    cli::cli_abort("Currently only the uilist return type is supported", call = .call)
  }

  .paginate <- as.integer(.paginate)
  if (!is.null(retmax) && !is.na(retmax) && retmax <= .paginate) .paginate <- 0L

  params <- rlang::list2(
    db = db,
    term = term,
    retstart = retstart,
    retmode = retmode,
    retmax = retmax,
    rettype = rettype,
    usehistory = if (usehistory) "y" else NULL,
    WebEnv = WebEnv,
    query_key = query_key,
    ...
  )

  if (usehistory) {
    params$retstart <- NULL
    params$retmax <- NULL
    req <- new_request("esearch.fcgi", params, .cookies = .cookies, .call = .call)
    doc <-
      httr2::req_perform(req, path = path_glue_dummy(.path)) |>
      parse_response("xml", call = .call)
    check_xml_root(doc, "eSearchResult")
    entrez_web_history(
      db = db,
      query_key = doc |> xml_find_first("/eSearchResult/QueryKey") |> xml_text(),
      WebEnv = doc |> xml_find_first("/eSearchResult/WebEnv") |> xml_text(),
      length = doc |> xml_find_first("/eSearchResult/Count") |> xml2::xml_integer()
    )
  } else if (.paginate == 0L) {
    if (is.na(retmax)) {
      cli::cli_abort("{.arg retmax} cannot be {.val NA} when pagination is disabled", call = .call)
    }
    req <- new_request("esearch.fcgi", params, .cookies = .cookies, .call = .call)
    ids <-
      httr2::req_perform(req, path = path_glue_dummy(.path)) |>
      parse_response("xml", call = .call) |>
      check_xml_eSearchResult(call = .call) |>
      process_response(fn = process_xml_eSearchResult_uilist, call = .call)
    entrez_id_list(db, ids)
  } else {
    params$retmax <- .paginate
    req <- new_request("esearch.fcgi", params, .cookies = .cookies, .call = .call)
    ids <-
      req |>
      httr2::req_perform_iterative(
        max_reqs = if (is.na(retmax)) Inf else ceiling((retmax - retstart) / .paginate),
        path = .path,
        on_error = "return",
        progress = "ESearch",
        next_req = httr2::iterate_with_offset(
          param_name = "retstart",
          start = retstart,
          offset = .paginate,
          resp_pages = function(resp) {
            n_items <- if (is.na(retmax)) {
              parse_response(resp, "xml", call = call) |>
                check_xml_eSearchResult(call = .call) |>
                process_response(fn = process_xml_eSearchResult_count, call = .call)
            } else {
              retmax - retstart
            }
            ceiling(n_items / .paginate)
          }
        )
      ) |>
      httr2::resps_successes() |>
      httr2::resps_data(function(resp) {
        parse_response(resp, "xml", call = .call) |>
          process_response(fn = process_xml_eSearchResult_uilist, call = .call)
      })
    
    entrez_id_list(db, ids)
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
    process_xml_eSearchResult_count()
}

check_xml_eSearchResult <- function(doc, call = rlang::caller_env()) {
  check_xml_root(doc, "eSearchResult", call = call)

  errors <- xml_find_all(doc, "//ErrorList/*")
  if (length(errors) > 0) {
    errors <- paste0("{.field ", xml2::xml_name(errors), "}: ", xml_text(errors))
    errors <- setNames(errors, rep("x", length(errors)))
  } else {
    errors <- c()
  }
  warnings <- xml_find_all(doc, "//WarningList/OutputMessage") |> xml_text()
  warnings <- setNames(warnings, rep("!", length(warnings)))
  msg <- c(errors, warnings)
  
  if (length(msg) > 0 ) {
    cli::cli_warn(c("eSearch returned messages", msg))
  }

  invisible(doc)
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