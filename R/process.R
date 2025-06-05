
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
NULL

# @param doc any parsed Entrez response document (e.g. {xml2} document or string)
# @return List of size 1 containing `doc`
process_identity <- function(doc) {
  list(doc)
}
.process_common <- new.env(parent = emptyenv())
.process_common$identity <- process_identity