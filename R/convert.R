#' Look up accessions and other IDs on Entrez
#' 
#' Passes the provided IDs through Entrez which has the effect of normalising the
#' accepted UIDs to the requested `idtype`, and rejecting invalid UIDs.
#' For web history lists, this forces results to be freshly downloaded
#' (unlike [`as_id_list()`] which can use cached results).
#' 
#' @family API methods
#' @param idtype if `NULL` return Entrez numeric IDs; if `"acc"` return accessions.
#' @inheritParams epost
#' @inheritParams efetch
#' @return [`id_list`] object
#' @export
entrez_translate <- function(
  id_set,
  idtype = NULL,
  .paginate = 5000L,
  .path = NULL,
  .call = current_env()
) {
  if (!.paginate) .paginate <- 5000L
  if (is_web_history(id_set)) {
    download_web_history(id_set, .paginate = .paginate, .path = .path, .call = .call)
  } else {
    ids <- efetch(
      id_set,
      retmode = "xml",
      rettype = "uilist",
      idtype = idtype,
      .process = function(doc) { doc |> xml_find_all("/IdList/Id") |> xml_text() },
      .paginate = .paginate,
      .path = .path,
      .call = .call
    )
    id_list(db = entrez_database(id_set), ids = ids)
  }
}

#' @include id_set.R
#' @rdname id_set
#' @inheritParams esearch
#' @export
as_id_list <- function(x, .paginate = 5000L, .path = NULL, .call = current_env()) {
  check_id_set(x)

  if (is_id_list(x)) return(x)
  
  ids <- wh_ids_get(x)
  if (is.null(ids)) {
    res <- download_web_history(x, .paginate = .paginate, .path = .path, .call = .call) 
    ids <- il_ids_get(res)
    wh_ids_set(x, ids)
  }

  id_list(entrez_database(x), ids)
}

download_web_history <- function(id_set, .paginate = 5000L, .path = NULL, .call = current_env()) {
  check_web_history(x)

  esearch(
    term = "",
    db = entrez_database(id_set),
    WebEnv = wh_webenv(id_set),
    query_key = wh_qrykey(id_set),
    usehistory = FALSE,
    .verbose = FALSE,
    .paginate = .paginate,
    .path = .path,
    .call = .call
  )
}