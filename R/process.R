
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
#' @name topic-process
#' @aliases process
#' @importFrom xml2 xml_text xml_find_first xml_find_all
NULL

# @param doc any parsed Entrez response document (e.g. {xml2} document or string)
# @return List of size 1 containing `doc`
process_identity <- function(doc) {
  list(doc)
}
.process_common <- new.env(parent = emptyenv())
.process_common$identity <- process_identity

process_response <- function(
  doc,
  fn,
  envir = .process_common,
  call = caller_env(),
  arg = rlang::caller_arg(fn)
) {
  fn <- as_function(fn, envir = envir, arg = arg, call = call)
  rlang::try_fetch(
    fn(doc),
    error = function(cnd) {
      cnd$call <- call(arg)
      rlang::abort("Failed to process API response", parent = cnd, call = call)
    }
  )
}

as_function <- function(
  x,
  envir = emptyenv(),
  arg = rlang::caller_arg(x),
  call = caller_env()
) {
  if (is.character(x)) {
    check_scalar_character_nonempty(x)
    if (exists(x, envir = envir, mode = "function")) {
      get(x, envir = envir, mode = "function")
    } else {
      allowed <- list_names(envir)
      msg <- "Choose from options: {.val {allowed}}, or provide a function"
      if (length(allowed) < 1L) msg <- "Provide a function instead"
      cli::cli_abort(c("{.val {x}} is not an allowed choice for {.arg {arg}}", "i" = msg), call = call)
    }
  } else if (rlang::is_function(x)) {
    return(x)
  } else {
    allowed <- list_names(envir)
    msg <- "Provide a function or choose from options: {.val {allowed}}"
    if (length(allowed) < 1L) msg <- "Provide a function instead"
    cli::cli_abort(c("{.arg {arg}} must be a function, not {.val {x}}", "i" = msg), call = call)
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

check_xml_root <- function(doc, expected, call = rlang::caller_env()) {
  root <- xml2::xml_root(doc) |> xml2::xml_name()
  if (root != expected) {
    cli::cli_abort(
      "Incorrect XML root node {.field <{root}>}, expecting {.field <{expected}>}",
      call = call
    )
  }
}

xml_text_from_list <- function(x) {
  purrr::map_chr(x, function(y) {
    y <- xml_text(y)
    if (length(y) > 0) y else NA_character_
  })
}

tibble_cnv <- function(df) {
  if (rlang::is_installed("tibble")) {
    tibble::as_tibble(df)
  }
}