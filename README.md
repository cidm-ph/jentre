
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jentre

<!-- badges: start -->

[![R-CMD-check](https://github.com/cidm-ph/jentre/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cidm-ph/jentre/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

jentre is a client for the NCBI’s Entrez API.

The Entrez API has many quirks. jentre attempts to deal with those while
presenting a convenient interface. It is designed for bulk metadata
fetching and link traversal, though also provides full access to other
parts of Entrez, albeit with fewer helpers.

Features of jentre:

- Provides objects representing sets of Entrez identifiers to avoid
  mixing them up
- Batches requests behind the scenes when needed
- Based on {httr2} and {xml2}

The {rentrez} package is more mature and might suit a broader set of
applications.

## Installation

You can install the development version of jentre like so:

``` r
# development version
install.packages('jentre', repos = c('https://cidm-ph.r-universe.dev', 'https://cloud.r-project.org'))
```

## Example

``` r
library(jentre)

# Searches by default use the Entrez history server for efficiency:
results <- esearch("Corynebacterium diphtheriae[orgn]", "biosample")
#> ℹ eSearch query "\"Corynebacterium diphtheriae\"[Organism]" has 4124 results
results
#> <entrez@/biosample[1]>
#> [1] MCID_68491.#1[4124]

# The returned object keeps track of which database the UIDs belong to,
# and whether the UIDs are local or on the history server. This makes
# them easier and less error-prone to use:
links <- elink(results, "sra")
links
#> # A tibble: 1 × 3
#>   from            linkname      to         
#>   <list>          <chr>         <list>     
#> 1 <idlst [4,124]> biosample_sra <wbhst [1]>

# You can pull a list of UIDs from the history server:
ids <- as_id_list(links$to[[1]])

# This is a vector with some extra metadata attached, but can be
# subsetted normally:
head(ids, n = 10)
#> <entrez/sra[10]>
#>  [1] 38889263 38768719 38768704 38641044 38501020 38428896 38428225 38427762
#>  [9] 38401944 38401943
as.character(ids[4:8])
#> [1] "38641044" "38501020" "38428896" "38428225" "38427762"

# For endpoints with richer data, you can provide an function to
# extract data you care about from the XML document. The output is
# combined if multiple API requests are needed, so you can end up
# with a single combined data frame, list, or vector:
esummary(
  ids[1:20],
  .process = function(doc) {
    xml2::xml_find_all(doc, "//DocumentSummary/CreateDate") |> xml2::xml_text()
  }
)
#>  [1] "2025/05/29" "2025/05/21" "2025/05/21" "2025/05/14" "2025/05/05"
#>  [6] "2025/04/30" "2025/04/30" "2025/04/30" "2025/04/29" "2025/04/29"
#> [11] "2025/04/29" "2025/04/29" "2025/04/29" "2025/04/29" "2025/04/29"
#> [16] "2025/04/29" "2025/04/29" "2025/04/21" "2025/04/21" "2025/04/21"

# If needed, you can construct an arbitrary request:
req <- entrez_request("esearch.fcgi", db = "nucleotide", term = "biomol+trna[prop]")
# You'll need to execute and parse it yourself:
httr2::req_perform(req) |> httr2::resp_body_xml()
#> {xml_document}
#> <eSearchResult>
#> [1] <Count>1012</Count>
#> [2] <RetMax>20</RetMax>
#> [3] <RetStart>0</RetStart>
#> [4] <IdList>\n  <Id>2737963026</Id>\n  <Id>2586967820</Id>\n  <Id>2274792564< ...
#> [5] <TranslationSet/>
#> [6] <TranslationStack>\n  <TermSet>\n    <Term>biomol+trna[prop]</Term>\n     ...
#> [7] <QueryTranslation>biomol+trna[prop]</QueryTranslation>
```
