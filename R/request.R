# FIXME If you use the E-utilities within software, NCBI's Disclaimer and
# Copyright notice (https://www.ncbi.nlm.nih.gov/About/disclaimer.html)
# must be evident to users of your product.

# FIXME To register tool and email values, simply send an e-mail to
#                     eutilities@ncbi.nlm.nih.gov
# including the desired values along with the name of either a developer or the
# organization creating the software.

#' Construct a request to the Entrez API
#' 
#' This is a low-level helper that builds a request object but does not
#' perform the request. In general you'll likely use higher-level methods
#' like [`efetch()`] instead.
#' 
#' Some parameters have special handling:
#'   * `api_key` is copied from the `ENTREZ_KEY` environment variable if set.
#'   * `email` and `tool` are set to default values but can be overridden.
#'   * `retmax` and `retstart` are always encoded in the URL query (not the body).
#' 
#' @section API limits:
#' The Entrez APIs are rate limited. Requests in this package try to respect the
#' limits (3/second, or 10/second when `api_key` is provided). Sometimes requests
#' still get rate limited, so they are retried up to 2 times (with backoff).
#' 
#' @param endpoint Entrez endpoint name (e.g. `"efetch.fcgi"`).
#' @param ... additional API parameters (refer to Entrez documentation).
#'   Any set to `NULL` are removed.
#' @param .method HTTP verb.
#' @param .multi controls how repeated params are handled (see [`httr2::req_url_query()`]).
#' @param .cookies path to persist cookies.
#'   If `NULL`, cookies are not added to the request.
#'   For helper functions: when `NA`, a temporary file is created (in this case only,
#'   the temporary file will be cleaned up once all requests are performed).
#' @param .call call environment to use in error messages/traces.
#'   You only need to specify this in internal helper functions that don't need to be
#'   mentioned in error messages.
#' @return `httr2::request` object.
#' @export
entrez_request <- function(endpoint, ..., .method = "GET", .multi = "comma", .cookies = NULL, .call = rlang::caller_env()) {
  new_request(
    endpoint,
    params = rlang::list2(...),
    .method = .method,
    .multi = .multi,
    .cookies = .cookies,
    .call = .call
  )
}

new_request <- function(endpoint, params, .method = "GET", .multi = "comma", .cookies = NULL, .call = rlang::caller_env()) {
  params <- params[!sapply(params, is.null, USE.NAMES = FALSE)]
  tool_params <- list(email = "Carl.Suster@health.nsw.gov.au", tool = "seqvalue")
  api_key <- Sys.getenv('ENTREZ_KEY')
  if (nchar(api_key) > 0) tool_params$api_key <- api_key
  params <- utils::modifyList(tool_params, params)

  dot_params <- names(params)[startsWith(names(params), ".")]
  if (length(dot_params) > 0) {
    cli::cli_abort(c(
      "Unknown Entrez params {dot_params}",
       "i" = "Are these misspelled arguments?"
    ), call = .call)
  }

  # always send these params in the URL query instead of form data
  query_param_names <- c("retmax", "retstart")

  rps <- 3L
  if (!is.null(params$api_key)) rps <- 10L

  req <-
    httr2::request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/") |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_method(.method) |>
    httr2::req_throttle(fill_time_s = 1L, capacity = rps - 1L) |>
    httr2::req_retry(max_tries = 3L, retry_on_failure = TRUE)

  if (!is.null(.cookies)) {
    req <- httr2::req_cookie_preserve(req, .cookies)
  }

  if (.method == "POST") {
    query_params <- params[intersect(query_param_names, names(params))]
    data <- params[setdiff(names(params), query_param_names)]
    req |>
      httr2::req_url_query(!!!query_params, .multi = "comma") |>
      httr2::req_body_form(!!!data, .multi = .multi)
  } else {
    req |> httr2::req_url_query(!!!params, .multi = .multi)
  }
}