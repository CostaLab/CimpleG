# Get CpG annotation from Illumina

Get CpG annotation from Illumina

## Usage

``` r
get_cpg_annotation(
  cpg_id,
  is_epic = TRUE,
  short_annotation = TRUE,
  silence_warnings = TRUE
)
```

## Arguments

- cpg_id:

  A character vector with the CpG IDs from Illumina to annotate.

- is_epic:

  A boolean, if TRUE, the annotation will be fetched from the EPIC
  array, otherwise from the 450k array. Default is TRUE.

- short_annotation:

  A boolean, if TRUE, only a small number of columns from the full
  annotation reference will be kept. This leads to an easier to read
  output. Default is TRUE.

- silence_warnings:

  A boolean, if TRUE, warnings produced during the downloading and
  loading of the data will be silenced. Default is TRUE.

## Value

A table with the annotated CpGs in the same order as the provided
signatures.

## Examples

``` r
# \donttest{
library("CimpleG")

# read data
signatures <- c("cg14501977", "cg24548498")

# Get signature annotation
signature_annotation <- get_cpg_annotation(signatures)
#> 
#> [CimpleG] Getting annotation manifest from Illumina.

# check signature annotation
signature_annotation
#> # A tibble: 2 × 8
#>   IlmnID     CHR_hg38 Start_hg38  End_hg38 UCSC_RefGene_Name UCSC_RefGene_Group
#>   <chr>      <chr>         <dbl>     <dbl> <chr>             <chr>             
#> 1 cg14501977 chr12     123948446 123948448 CCDC92            5'UTR             
#> 2 cg24548498 chr2      181684680 181684682 NA                NA                
#> # ℹ 2 more variables: UCSC_CpG_Islands_Name <chr>,
#> #   Relation_to_UCSC_CpG_Island <chr>
# }
```
