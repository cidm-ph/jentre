#' Construct a request to the Entrez API
#'
#' This is a low-level helper that builds a request object but does not
#' perform the request. In general you'll likely use higher-level methods
#' like [`efetch()`] instead.
#'
#' `email`, `tool`, and `api_key` have default values but these can be
#' overridden, or can be removed by setting them to `NULL`.
#'
#' @section API limits:
#' The Entrez APIs are rate limited.
#' Requests in this package respect the API headers returned by Entrez.
#' Without an API key you will be rate limited more aggressively, so it is
#' recommended to [obtain an API key][entrez api key].
#' `jentre` searches for the API key in the following order:
#'   * the API parameter `entrez_key` provided to any API request function,
#'   * the [option][base::options] `"jentre.api_key"`, then
#'   * the environment variable `ENTREZ_KEY`.
#'
#' You can check the value is found properly using `entrez_api_key()`.
#' If no API key is set, a warning will be displayed. This can be suppressed
#' by setting the option `"jentre.silence_api_warning"` to `TRUE`.
#'
#' [entrez api key]: https://support.nlm.nih.gov/kbArticle/?pn=KA-05317
#'
#' @param endpoint Entrez endpoint name (e.g. `"efetch.fcgi"`).
#' @param ... additional API parameters (refer to Entrez documentation).
#'   Any set to `NULL` are removed.
#' @param .method HTTP verb.
#'   For `"POST"`, any params with vector values (usually just `id`) are sent in the
#'   request body instead of the URL.
#' @param .multi controls how repeated params are handled (see [`httr2::req_url_query()`]).
#' @param .cookies path to persist cookies.
#'   If `NULL`, cookies are not added to the request.
#'   For helper functions: when `NA`, a temporary file is created (in this case only,
#'   the temporary file will be cleaned up once all requests are performed).
#' @param .verbose logical: when TRUE logs all API requests as messages in a compact format.
#'   This uses a summarised format that does not include the request body for POST.
#'   Use normal httr verbosity controls (e.g. [`httr2::local_verbosity()`]) to override
#'   this behaviour and see more details.
#' @param .call call environment to use in error messages/traces.
#'   See [rlang::topic-error-call] and the `call` argument of [cli::cli_abort()].
#'   You only need to specify this in internal helper functions that don't need to be
#'   mentioned in error messages.
#' @return
#'  * for `entrez_request()` an `httr2::request` object.
#'  * for `entrez_api_key()`, the API key as a character, or `default` if no global config exists.
#' @export
entrez_request <- function(
  endpoint,
  ...,
  .method = "GET",
  .multi = "comma",
  .cookies = NULL,
  .verbose = getOption("jentre.verbose", default = TRUE),
  .call = current_env()
) {
  new_request(
    endpoint,
    params = rlang::list2(...),
    .method = .method,
    .multi = .multi,
    .cookies = .cookies,
    .verbose = .verbose,
    .call = .call
  )
}

new_request <- function(
  endpoint,
  params,
  .method = "GET",
  .multi = "comma",
  .body_params = NULL,
  .cookies = NULL,
  .verbose = getOption("jentre.verbose", default = TRUE),
  .call = caller_env()
) {
  # set default params but allow them to be overridden (or removed with NULL values)
  tool_params <- list(
    email = "Carl.Suster@health.nsw.gov.au",
    tool = "jentre",
    api_key = entrez_api_key()
  )
  params <- utils::modifyList(tool_params, params)

  # remove any NULL-values params
  params <- params[!sapply(params, is.null, USE.NAMES = FALSE)]

  dot_params <- names(params)[startsWith(names(params), ".")]
  if (length(dot_params) > 0) {
    known_dot_params <- rlang::fn_fmls_names(entrez_request)
    known_dot_params <- known_dot_params[startsWith(known_dot_params, ".") & (known_dot_params != "...")]
    known_dot_params <- setdiff(known_dot_params, dot_params)
    cli::cli_abort(c(
      "Unknown Entrez param{?s} {.field {dot_params}}",
      "i" = "Did you mean {.or {.arg {known_dot_params}}}?"
    ), call = .call)
  }

  if (is.null(params$api_key) && !getOption("jentre.silence_api_warning", FALSE)) {
    cli::cli_alert_warning("No API key was provided. Set {.env ENTREZ_KEY} to reduce rate limiting.")
    cli::cli_alert_info("More info on API keys: https://support.nlm.nih.gov/kbArticle/?pn=KA-05317")
  }

  if (!is.null(.body_params) && .method != "POST") {
    cli::cli_abort(c(
      "Only the {.val POST} HTTP method is compatible with {.arg .body_params}",
      "i" = "You specified a {.arg .method} of {.val { .method}}"
    ), call = .call)
  }
  .body_params <- intersect(as.character(.body_params), names(params))

  req <-
    httr2::request("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/") |>
    httr2::req_user_agent("jentre (https://github.com/cidm-ph/jentre)") |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_method(.method) |>
    httr2::req_retry(max_tries = 3L, retry_on_failure = TRUE)

  if (.verbose) {
    req <- httr2::req_options(req, debugfunction = debug_request, verbose = TRUE)
  }

  if (!is.null(.cookies) && !is.na(.cookies)) {
    req <- httr2::req_cookie_preserve(req, .cookies)
  }

  if (.method == "POST") {
    data_params <- names(params)[Map(length, params) > 1L]
    data_params <- union(data_params, .body_params)
    query_params <- setdiff(names(params), data_params)
    req |>
      httr2::req_url_query(!!!params[query_params], .multi = "error") |>
      httr2::req_body_form(!!!params[data_params], .multi = .multi)
  } else {
    req |> httr2::req_url_query(!!!params, .multi = .multi)
  }
}

#' @rdname entrez_request
#' @param default default value to return if no global configuration is found.
#' @export
entrez_api_key <- function(default = NULL) {
  api_key <- getOption("jentre.entrez_key")
  if (rlang::is_zap(api_key)) return(default)
  if (!is.null(api_key)) return(api_key)

  api_key <- Sys.getenv("ENTREZ_KEY")
  if (nchar(api_key) > 0) return(api_key)

  default
}

.info <- rlang::new_environment()

debug_request <- function(type, msg) {
  if (type == 2) { # req header
    x <- readBin(msg, character())
    lines <- unlist(strsplit(x, "\r?\n", useBytes = TRUE))
    url <- httr2::url_parse(
      strsplit(lines[[1]], " ", fixed = TRUE)[[1]][[2]],
      "https://eutils.ncbi.nlm.nih.gov"
    )
    .info$endpoint <- gsub("\\.fcgi$", "", basename(url$path))
    .info$data <- url$query
    .info$body <- character()
  } else if (type == 4) { # req body
    x <- readBin(msg, character())
    .info$body <- c(.info$body, x)
  } else if (type == 1) { # resp headers
    x <- readBin(msg, character())
    lines <- unlist(strsplit(x, "\r?\n", useBytes = TRUE))
    if (startsWith(lines[[1]], "HTTP")) {
      msg <- "{.strong {(.info$endpoint)}}"
      parm <- .info$data[setdiff(names(.info$data), c("tool", "email", "api_key"))]
      if (length(parm) > 0L) {
        msg <- paste0(msg, " ", format_kv(parm))
      }

      req_body <- list()
      if (length(.info$body) > 0L) {
        req_body <- httr2::url_query_parse(.info$body)
      }
      if (length(req_body) > 0L) {
        msg <- paste0(msg, " {cli::symbol$star} ", format_kv(req_body))
      }
      cli::cli_alert(msg)
    }
  }
}

format_kv <- function(x) {
  keys <- names(x)
  vals <- format(unname(x))
  suff <- rep("", length(x))

  # deal with id="a,b,c,d"
  commas <- gregexpr(",", vals)
  commas <- regmatches(vals, commas) |> lengths()
  vals <- gsub(",.*,", ",{cli::symbol$ellipsis},", vals)
  suff[commas > 0L] <-  paste0("[", commas[commas > 0L] + 1L, "]")

  # deal with id="a" id="b" id="c" id="d"
  group_l <- keys == keys[seq_along(keys) + 1]
  group_r <- keys == c(NA_character_, keys[seq_along(keys) - 1])
  group_l[is.na(group_l)] <- FALSE
  group_r[is.na(group_r)] <- FALSE
  group_n <- sapply(keys, \(x) sum(keys == x) - 2)
  group_start <- group_l & (group_n > 1L)
  elide <- group_l & group_r & (group_n > 1L)
  suff[group_start] <- paste0(" {cli::symbol$ellipsis}{cli::symbol$times}", group_n[group_start])
  keys <- keys[!elide]
  vals <- vals[!elide]
  suff <- suff[!elide]

  paste0(
    paste0("{.field ", keys, "}"),
    "=",
    paste0("{.val ", vals, "}"),
    suff,
    collapse = " "
  )
}