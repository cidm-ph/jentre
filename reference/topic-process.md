# Process API results

Function to turn the parsed response document into meaningful data. It
must accept one argument, `doc`, the parsed response document. The
return value must be compatible with
[`vctrs::list_unchop()`](https://vctrs.r-lib.org/reference/vec_chop.html),
e.g. a vector, list, or data frame.

## Details

API results are parsed based on the `retmode` parameter. XML documents
will be parsed into `xml2::xml_document` objects and an error will be
raised if it contains an `<ERROR>` node.

Builtin processors can be referred to by name instead of specifying your
own function. Some helpers provide additional processors, but these are
always available:

- `"identity"`: Puts the parsed output document into a list. Where
  multiple requests are made (e.g. using the batched APIs like
  [`efetch()`](https://cidm-ph.github.io/jentre/reference/efetch.md))
  these will then be concatenated into a single list.
