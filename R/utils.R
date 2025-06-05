#' @importFrom xml2 xml_text xml_find_first xml_find_all
NULL

xml_text_from_list <- function(x) {
  purrr::map_chr(x, function(y) {
    y <- xml_text(y)
    if (length(y) > 0) y else NA_character_
  })
}

parse_response <- function(resp, retmode, ..., errors = TRUE, call = rlang::caller_env()) {
  # TODO handle application errors
  doc <- switch(
    retmode,
    xml = httr2::resp_body_xml(resp, ...),
    json = httr2::resp_body_json(resp, ...),
    httr2::resp_body_string(resp, ...)
  ) 
  if (errors && retmode == "xml") raise_xml_error(doc, call = call)
  doc
}

raise_xml_error <- function(doc, call = rlang::caller_env()) {
  err <- xml_find_first(doc, "//ERROR")
  if (!is.na(err)) {
    cli::cli_abort(
      c("Entrez error encountered in XML response", "x" = xml_text(err)),
      call = call
    )
  }
  doc
}

entrez_id_params <- function(id_set) {
  if (is.entrez_id_list(id_set)) {
    list(db = id_set$database, id = id_set$ids)
  } else if (is.entrez_web_history(id_set)) {
    list(db = id_set$database, query_key = id_set$query_key, WebEnv = id_set$WebEnv)
  } else {
    stop("Unimplemented")
  }
}

tibble_cnv <- function(df) {
  if (rlang::is_installed("tibble")) {
    tibble::as_tibble(df)
  }
}

list_names <- function(env) {
  names <- character()
  while (!identical(env, emptyenv())) {
    names <- c(names, ls(env))
    env <- parent.env(env)
  }
  sort(names)
}

as_function <- function(x, env = emptyenv(), arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (is.character(x)) {
    if (exists(x, envir = env, mode = "function")) {
      get(x, envir = env, mode = "function")
    } else {
      cli::cli_abort(c(
        "{.val {x}} is not an allowed choice for {.arg {arg}}",
        "i" = "Choose from options: {.val {list_names(env)}}, or provide a function"
      ), call = call)
    }
  } else if (rlang::is_function(x)) {
    return(x)
  } else {
    cli::cli_abort(c(
      "{.arg {arg}} must be a function, not {.val {x}}",
        "i" = "Provide a function or choose from options: {.val {list_names(env)}}"
    ), call = call)
  }
}

#' Process API results
#' 
#' Function to turn the parsed response document into meaningful data.
#' It must accept one argument, `doc`, the parsed response document. 
#' The return value must be compatible with [`vctrs::list_unchop()`],
#' e.g. a vector, list, or data frame.
#' 
#' API results are parsed based on the `retmode` parameter. XML documents will
#' be parsed into `xml2::xml_document` objects and an error will be raised if
#' it contains an `<ERROR>` node.
#' 
#' Builtin processors can be referred to by name instead of specifying your own
#' function. Some helpers provide additional processors, but these are always
#' available:
#' 
#'   - `"identity"`:
#'     Puts the parsed output document into a list. Where multiple requests are made
#'     (e.g. using the batched APIs like [`efetch()`]) these will then be
#'     concatenated into a single list.
#' 
#' @rdname process
#' @param doc any parsed Entrez response document (e.g. {xml2} document or string)
#' @return List of size 1 containing `doc`
#' @export
process_identity <- function(doc) {
  list(doc)
}
.process_common <- new.env(parent = emptyenv())
.process_common$identity <- process_identity