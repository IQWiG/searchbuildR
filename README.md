
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

After installing the package you can launch the Shiny App locally in R:

``` r
library(searchbuildR)
run_app()
```

The interface looks roughly like this (version 0.0.11):

<img src="inst/extdata/screenshot_searchbuildR.png"/>

### Questions, suggestions and bugs

Ideas and suggestions for new functionalities or hints towards any bugs
are welcome. Simply open an issue at
<https://github.com/IQWiG/searchbuildR>.
