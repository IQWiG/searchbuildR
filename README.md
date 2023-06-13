
<!-- README.md is generated from README.Rmd. Please edit that file -->

# searchbuildR

<!-- badges: start -->
<!-- badges: end -->

The goal of searchbuildR is to identify overrepresented terms in a set
of relevant references for a systematic review or evidence synthesis in
general, which can then be applied in a boolean search in a bibliograpic
database (e.g. PubMed).

## Installation

You can install searchbuildR from GitHub. The package is currently not
released on CRAN, but will be in the future.

``` r
library(remotes)
remotes::install_github("https://github.com/IQWiG/searchbuildR")
```

## Example

This is a basic example which shows you how to solve a common problem,
the package was designed for:

``` r
library(searchbuildR)
path <- system.file("extdata", "example_ris.txt", package= "searchbuildR", mustWork = T)
data <- z_scores(path)
head(print_z_scores(data, terms = "freetext") )
#>      Wort Wortfrequenz Anzahl Referenzen Erwartete Frequenz   Z-Score
#> 1     ris            2                 1       7.067791e-05 237.88861
#> 2    file            2                 1       1.734821e-04 151.83330
#> 3 package            1                 1       2.248843e-04  66.66924
#> 4    text            1                 1       6.875033e-04  38.11292
#> 5 typical            1                 1       1.452110e-03  26.20515
#> 6    here            1                 1       6.791505e-03  12.05423
#>   Approximationskriterium zutreffend?
#> 1                               FALSE
#> 2                               FALSE
#> 3                               FALSE
#> 4                               FALSE
#> 5                               FALSE
#> 6                               FALSE
head(print_z_scores(data, terms = "MeSH"))
#> # A tibble: 3 × 5
#>   MeSH                        Frequenz `Erwartete Frequenz` `Z-Score` Approxim…¹
#>   <chr>                          <int>                <dbl>     <dbl> <lgl>     
#> 1 Systematic Reviews as Topic        1            0.0000513    140.   FALSE     
#> 2 Adult                              1            0.0515         4.22 FALSE     
#> 3 Library Automation                 1           NA          10000    NA        
#> # … with abbreviated variable name ¹​`Approximationskriterium zutreffend?`
```

### Questions, suggestions and bugs

Ideas and suggestions for new functionalities or hints towards any bugs
are welcome. Simply open an issue at
<https://github.com/IQWiG/searchbuildR>.
