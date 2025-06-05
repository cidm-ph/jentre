
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jentre

<!-- badges: start -->

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

# Searches by default use the Entrez history server:
results <- esearch("vibrio cholerae[orgn]", "biosample")
# <entrez@/biosample d085e#1 [31496 UIDs]>

links <- elink(results, "bioproject")
# # A tibble: 2 × 3
#   from       linkname                 to        
#   <list>     <chr>                    <list>    
# 1 <entrz_d_> biosample_bioproject     <entrz_w_>
# 2 <entrz_d_> biosample_bioproject_all <entrz_w_>

# You can force one-to-one links:
elink_map(y, "bioproject", linkname = "biosample_bioproject")
# # A tibble: 7 × 5
#   db_from    id_from db_to      linkname             id_to    
#   <chr>      <chr>   <chr>      <chr>                <list>   
# 1 bioproject 1086945 bioproject biosample_bioproject <chr [1]>
# 2 bioproject 610963  bioproject biosample_bioproject <chr [2]>
# 3 bioproject 610543  bioproject biosample_bioproject <chr [2]>

# History server results can be converted into explicit ID lists:
ids <- entrez_translate(links$to[[1]])
# <entrez/bioproject [28 UIDs]>
head(ids, n = 10)
# <entrez/bioproject [10 UIDs]>
entrez_ids(ids[4:8])
# [1] "1261856" "1258490" "1253705" "1247499" "1246491"


# If needed, you can construct an arbitrary request
req <- entrez_request("esearch.fcgi", db = "nucleotide", term = "biomol+trna[prop]")
# you'll need to execute and parse it yourself:
httr2::req_perform(req) |> httr2::resp_body_xml()
```
