#' Count the number of entries in an ID set
#' 
#' If `id_set` is an [`id_list`] then this is equivalent to `length()`.
#' If it is a `web_history`, this may involve an Entrez API call to get the
#' number of entries. In this case the result is cached so that subsequent
#' calls don't hit the API again.
#'
#' @param id_set an ID set object.
#' @return integer number of entries.
#' @inheritParams esearch
#' @export
entrez_count <- function(id_set, .call = current_env()) {
  check_id_set(id_set, call = .call)

  if (is_id_list(id_set)) {
    return(vctrs::vec_size(id_set))
  }

  len <- wh_len_get(id_set)
  if (is.na(len)) {
    len <- entrez_count_online(id_set, .call = .call)
    wh_len_set(id_set, len)
  }
  len
}

entrez_count_online <- function(id_set, .call = caller_env()) {
  check_web_history(id_set, call = .call)

  params <- rlang::list2(
    db = entrez_database(id_set),
    term = "",
    retmode = "xml",
    rettype = "count",
    WebEnv = wh_webenv(id_set),
    query_key = wh_qrykey(id_set)
  )
  req <- new_request("esearch.fcgi", params, .call = .call)

  req |>
    httr2::req_perform() |>
    parse_response("xml", call = .call) |>
    process_xml_eSearchResult_count()
}