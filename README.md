# NAFOdown

<!-- badges: start -->
<!-- badges: end -->

NAFOdown is an R package that was built to simplify the process of creating NAFO Scientific Council (SC) documents. This package is based on (csasdown)[https://github.com/pbs-assess/csasdown].

## Installation

You can install NAFOdown via github:

``` r
# install.packages("devtools")
devtools::install_github("PaulRegular/NAFOdown")
```

## Example

Once the package is installed, this line of code will produce a skeleton of the files and folders for producing a NAFO SCR:

``` r
NAFOdown::draft("SCR")
```

and this line will produce the skeleton of a STACFIS report:

``` r
NAFOdown::draft("STACFIS")
```

You'll need to edit the individual Rmd files to produce your report. To render your document, open `index.Rmd` in RStudio and click the "knit" button.


