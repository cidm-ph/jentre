#' ELink API for fetching links between databases
#' 
#' `elink()` offers direct access to the ELink API endpoint, which has many different
#' input and output formats depending on parameters. If you just want a one-to-one
#' mapping of `neighbor` links, use `elink_map()`, which handles this for you.
#' 
#' @section One-to-one mapping:
#' Note that some ways of calling this API on multiple UIDs result in the one-to-one
#' association of the input and output sets getting lost. The way around this is to
#' specify each ID as a separate parameter rather than a single comma-separated param.
#' This is handled by the default choice of `.multi = "explode"`. When using a web
#' history token as input, there is no corresponding way to ensure one-to-one mapping.
#' To ensure that the result is always one-to-one, use `elink_map()`, which may make
#' several API requests to achieve the result.
#' 
#' @family API methods
#' @inheritParams entrez_request
#' @param id_set ID set object.
#' @param db target database name.
#' @param cmd ELink command.
#'   If `NA` either `"neighbor"` or `"neighbor_history"` will be used based on the
#'   type of input.
#' @param retmode response format.
#' @param .paginate maximum number of UIDs to submit per request.
#'   `.paginate` only applies when `id_set` is an explicit list.
#'   Set to `FALSE` to prevent batching.
#' @param .process function that processes the API results.
#'   Can be a function or builtin processor as described in [`process`].
#'   Additional builtin processors are available:
#'     `"sets"` to get a data frame of ID set objects,
#'     `"flat"` to get a data frame of UIDs, or
#'     `NA` to use a sensible choice based on parameters.
#' @param .progress controls progress bar; see the `progress` argument of
#'   [`httr2::req_perform_iterative()`].
#' @param .path path specification for saving raw responses.
#'   See `path` argument of [`httr2::req_perform_iterative()`].
#' @return concatenated output of `.process`.
#'   For `elink(.process = "sets")` a data frame with columns
#'   \describe{
#'     \item{\code{from}}{Source link set.}
#'     \item{\code{to}}{Target link set.}
#'     \item{\code{linkname}}{Link name (see \url{https://eutils.ncbi.nlm.nih.gov/entrez/query/static/entrezlinks.html} or \code{\link{einfo}}).}
#'   }
#'   For `elink_map()` and `elink(.process = "flat")` a data frame with columns
#'   \describe{
#'     \item{\code{db_from}}{Source database name.}
#'     \item{\code{id_from}}{Source identifier. Can be a list column depending on how \code{\link{elink}} was called.}
#'     \item{\code{db_to}}{Target database name.}
#'     \item{\code{linkname}}{Link name (see \url{https://eutils.ncbi.nlm.nih.gov/entrez/query/static/entrezlinks.html} or \code{\link{einfo}}).}
#'     \item{\code{id_to}}{Target identifier. In general this will be a list column.}
#'   }
#' @export
elink <- function(
  id_set,
  db,
  ...,
  retmode = "xml",
  cmd = NA,
  .paginate = 100L,
  .process = NA,
  .method = NA,
  .multi = "explode",
  .progress = TRUE,
  .cookies = NA,
  .path = NULL,
  .call = current_env()
) {
  check_id_set(id_set)

  id_params <- entrez_id_params(id_set)
  id_params$dbfrom <- id_params$db
  id_params$db <- NULL

  if (is.na(cmd)) cmd <- if (is_web_history(id_set)) "neighbor_history" else "neighbor"
  if (is.na(.method)) .method <- if (is_web_history(id_set)) "GET" else "POST"

  if (rlang::is_na(.process)) {
    .process <- if (is_id_list(id_set) && cmd == "neighbor" && .multi == "explode") {
      process_xml_eLinkResult_flat
    } else if (cmd %in% c("neighbor", "neighbor_history")) {
      process_xml_eLinkResult_sets
    } else {
      process_identity
    }
  }

  .paginate <- as.integer(.paginate)
  if (is_id_list(id_set) && length(id_set) <= .paginate) .paginate <- 0L

  params <- rlang::list2(
    db = db,
    !!!id_params,
    retmode = retmode,
    cmd = cmd,
    ...
  )

  if (is_id_list(id_set) && .paginate > 0L) {
    if (is.na(.cookies)) {
      .cookies <- tempfile()
      on.exit(if (file.exists(.cookies)) file.remove(.cookies), add = TRUE)
    }

    sets <- split_id_list(id_set, max_per_batch = .paginate)
    params$id <- il_ids_get(sets[[1]])
    req <- new_request(
      "elink.fcgi", params, .method = "POST", .body_params = c("id"),
      .cookies = .cookies, .call = .call
    )

    req |>
      httr2::req_perform_iterative(
        next_req = iterate_body_form(
          id = Map(il_ids_get, sets[2:length(sets)]),
          .multi = .multi,
          .call = .call
        ),
        path = .path,
        max_reqs = length(sets),
        on_error = "return",
        progress = .progress
      ) |>
      httr2::resps_successes() |>
      httr2::resps_data(function(resp) {
        parse_response(resp, retmode, call = .call) |>
          process_response(fn = .process, envir = .process_elink, call = .call, arg = ".process")
      })
  } else {
    req <- new_request(
      "elink.fcgi", params, .method = .method, .multi = .multi,
      .cookies = .cookies, .call = .call
    )
    resp <- httr2::req_perform(req, path = path_glue_dummy(.path))
    doc <- parse_response(resp, retmode, call = .call)

    # For some reason, Entrez returns the full set of IDs in the web history item as
    # part of its response. We may as well cache that to make any future conversion
    # to an ID list instant.
    if (is_web_history(id_set) && retmode == "xml") {
      from <- process_xml_eLinkResult_from(doc)
      wh_ids_set(id_set, il_ids_get(from))
    }

    process_response(doc, fn = .process, envir = .process_elink, call = .call, arg = ".process")
  }
}

#' @rdname elink
#' @export
elink_map <- function(
  id_set,
  db,
  ...,
  .cookies = NA,
  .path = NULL,
  .call = current_env()
) {
  check_id_set(id_set)

  if (is.na(.cookies)) {
    .cookies <- tempfile()
    on.exit(if (file.exists(.cookies)) file.remove(.cookies), add = TRUE)
  }

  if (is_id_list(id_set)) {
    elink(
      id_set,
      db,
      ...,
      cmd = "neighbor",
      retmode = "xml",
      .method = "POST",
      .multi = "explode",
      .process = process_xml_eLinkResult_flat,
      .cookies = .cookies,
      .path = .path,
      .call = .call
    ) |> tibble_cnv()
  } else {
    params <- rlang::list2(...)
    elink(
      id_set,
      db,
      !!!params,
      cmd = "neighbor",
      retmode = "xml",
      .method = "GET",
      .multi = "comma",
      .process = process_xml_eLinkResult_sets,
      .cookies = .cookies,
      .path = .path,
      .call = .call
    ) |>
      purrr::pmap(function(from, linkname, to) {
        remap_links(from, linkname, to, params, .cookies, .call)
      }) |>
      purrr::list_rbind() |>
      tibble_cnv()
  }
}

# from - entrez_id_list
#   to - entrez_web_history
remap_links <- function(from, linkname, to, params, .cookies, .call) {
  to <- as_id_list(to)

  params$linkname <- linkname
  if (length(to) < 0.75 * length(from)) {
    # the reverse relationship will be more efficient
    check_id_list(to)
    elink_map(to, entrez_database(from), !!!params, .cookies = .cookies, .call = .call)
  } else {
    check_id_list(from)
    elink_map(from, entrez_database(to), !!!params, .cookies = .cookies, .call = .call)
  }
}

process_xml_eLinkResult_flat <- function(doc) {
  check_xml_root(doc, "eLinkResult")
  n_linkset <- doc |> xml_find_all("/eLinkResult/LinkSet") |> length()
  n_source <- doc |> xml_find_all("/eLinkResult/LinkSet/IdList/Id") |> length()

  if (!is.na(xml_find_first(doc, "//LinkSetDbHistory"))) {
    cli::cli_abort("Can't process history server result into a flat data frame")
  }

  if (n_linkset == n_source) {
    process_xml_LinkSet_df_one_to_one(doc)
  } else {
    process_xml_LinkSet_df_many_to_many(doc)
  } |> tibble_cnv()
}

process_xml_LinkSet_df_one_to_one <- function(doc) {
  links <- xml_find_all(doc, "LinkSet/LinkSetDb")
  vctrs::data_frame(
    db_from = links |> xml2::xml_find_first("./ancestor::LinkSet/DbFrom") |> xml_text(),
    id_from = links |> xml_find_all("./ancestor::LinkSet/IdList/Id", flatten = FALSE) |> xml_text_from_list() |> as.list(),
    db_to = links |> xml_find_all("./DbTo") |> xml_text(),
    linkname = links |> xml_find_all("./LinkName") |> xml_text(),
    id_to = links |> xml_find_all("./Link/Id", flatten = FALSE) |> purrr::map(xml_text),
  )
}

process_xml_LinkSet_df_many_to_many <- function(doc) {
  links <- xml_find_all(doc, "LinkSet/LinkSetDb")
  vctrs::data_frame(
    db_from = links |> xml2::xml_find_first("./ancestor::LinkSet/DbFrom") |> xml_text(),
    id_from = doc |> xml_find_all("/eLinkResult/LinkSet/IdList/Id") |> xml_text() |> list(),
    db_to = links |> xml_find_all("./DbTo") |> xml_text(),
    linkname = links |> xml_find_all("./LinkName") |> xml_text(),
    id_to = links |> xml_find_all("./Link/Id", flatten = FALSE) |> purrr::map(xml_text),
  )
}

process_xml_eLinkResult_from <- function(doc) {
  check_xml_root(doc, "eLinkResult")
  dbfrom <- xml_find_first(doc, "/eLinkResult/LinkSet/DbFrom") |> xml_text()
  idsfrom <- xml_find_all(doc, "/eLinkResult/LinkSet/IdList/Id") |> xml_text()
  id_list(dbfrom, idsfrom)
}

process_xml_eLinkResult_sets <- function(doc) {
  check_xml_root(doc, "eLinkResult")
  sets <- xml_find_all(doc, "/eLinkResult/LinkSet")
  purrr::map(sets, function(set) {
    # source set is always an explicit list, even when user input was a WebEnv
    dbfrom <- xml_find_first(set, "./DbFrom") |> xml_text()
    idsfrom <- xml_find_all(set, "./IdList/Id") |> xml_text()
    from <- id_list(dbfrom, idsfrom)

    setdb <- xml_find_all(set, "./LinkSetDb", flatten = FALSE)
    dbto <- xml_find_all(setdb, "./DbTo", flatten = FALSE) |> xml_text_from_list()

    ret <- vctrs::data_frame(
      from = list(from),
      linkname = xml_find_all(setdb, "./LinkName", flatten = FALSE) |> xml_text_from_list(),
      to = purrr::map2(setdb, dbto, function(x, y) {
        id_list(y, xml_find_all(x, "./Link/Id", flatten = FALSE) |> xml_text_from_list())
      })
    )

    WebEnv <- xml_find_first(set, "./WebEnv") |> xml_text()
    if (is.na(WebEnv)) return(ret)
    dbto <- xml_find_all(set, "./LinkSetDbHistory/DbTo", flatten = FALSE) |> xml_text_from_list()
    linkname <- xml_find_all(set, "./LinkSetDbHistory/LinkName", flatten = FALSE) |> xml_text_from_list()
    query_key <- xml_find_all(set, "./LinkSetDbHistory/QueryKey", flatten = FALSE) |> xml_text_from_list()

    vctrs::data_frame(
      from = list(from),
      linkname = linkname,
      to = purrr::map2(dbto, query_key, \(x, y) web_history(x, WebEnv, y))
    ) |> vctrs::vec_rbind(ret)
  }) |>
    purrr::list_rbind() |>
    tibble_cnv()
}

#' @include process.R
.process_elink <- new.env(parent = .process_common)
.process_elink$sets <- process_xml_eLinkResult_sets
.process_elink$flat <- process_xml_eLinkResult_flat