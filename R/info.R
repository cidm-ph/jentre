#' Get details about Entrez databases
#' 
#' These functions call the EInfo endpoint. `einfo()` provides the number
#' of entries in the databases, the name and description, list of terms
#' usable in the query syntax, and list of link names usable with the ELink
#' endpoint.
#' 
#' @param db name of database to provide information about.
#' @param retmode response format.
#' @param version response format version.
#' @inheritParams entrez_request
#' @return Character vector of database names for `einfo_databases()`.
#'   An XML document with root node `<eInfoResult>` for `einfo()`.
#' @family API methods
#' @export
einfo <- function(db, ..., retmode = "xml", version = "2.0", .call = rlang::caller_env()) {
  params <- rlang::list2(
    db = db,
    retmode = retmode,
    version = version,
    ...
  )
  req <- new_request("einfo.fcgi", params, .call = .call)

  httr2::req_perform(req) |> parse_response(retmode, call = .call)
}

#' @rdname einfo
#' @export
einfo_databases <- function(..., retmode = "xml", .call = rlang::caller_env()) {
  if (retmode != "xml") {
    cli::cli_abort("Currently only XML format is supported", call = .call)
  }

  params <- rlang::list2(
    retmode = retmode,
    ...
  )
  req <- new_request("einfo.fcgi", params, .call = .call)

  req |>
    httr2::req_perform() |>
    parse_response(retmode, call = .call) |>
    xml_find_all("//DbName") |>
    xml_text()
}