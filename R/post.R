#' Register UIDs with the Entrez history server
#' 
#' @param id_set an [`entrez_id_list`] object.
#' @param WebEnv either a character to pass on as-is, or an [`entrez_web_history`] object.
#' @inheritParams entrez_request
#' @param .path path specification for saving raw responses.
#' @family API methods
#' @export
epost <- function(id_set, ..., WebEnv = NULL, .path = NULL, .call = rlang::current_env()) {
  check_id_list(id_set)

  ids <- il_ids_get(id_set)
  db <- entrez_database(id_set)

  params <- rlang::list2(
    db = db,
    id = ids,
    WebEnv = WebEnv,
    !!!webhist_params(WebEnv = WebEnv),
    ...
  )
  req <- new_request("epost.fcgi", params, .method = "POST", .call = .call)

  doc <-
    httr2::req_perform(req, path = path_glue_dummy(.path)) |>
    parse_response("xml", call = .call)
  web_history(
    db = db,
    query_key = doc |> xml2::xml_find_first("//QueryKey") |> xml2::xml_text(),
    WebEnv = doc |> xml2::xml_find_first("//WebEnv") |> xml2::xml_text(),
    length = length(ids)
  )
}

#' Convert web history result or accessions and other IDs to Entrez UIDs
#' 
#' @family API methods
#' @inheritParams epost
#' @inheritParams efetch
#' @export
entrez_translate <- function(id_set, .paginate = 5000L, .path = NULL, .call = rlang::current_env()) {
  if (!.paginate) .paginate <- 5000L
  if (is.entrez_web_history(id_set)) {
    esearch(
      term = "",
      db = entrez_database(id_set),
      WebEnv = id_set$WebEnv,
      query_key = id_set$query_key,
      usehistory = FALSE,
      .paginate = .paginate,
      .path = .path,
      .call = .call
    )
  } else {
    ids <- efetch(
      id_set,
      retmode = "xml",
      rettype = "uilist",
      .process = function(doc) { doc |> xml_find_all("/IdList/Id") |> xml_text() },
      .paginate = .paginate,
      .path = .path,
      .call = .call
    )
    entrez_id_list(db = entrez_database(id_set), ids = ids)
  }
}