
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tabulate

<!-- badges: start 

[![CRAN](https://img.shields.io/cran/v/mikropml?color=blue&label=CRAN&logo=R)](https://CRAN.R-project.org/package=mikropml)
[![Conda](https://img.shields.io/conda/vn/conda-forge/r-mikropml)](https://anaconda.org/conda-forge/r-mikropml)
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/mikropml)

-->

[![R-CMD-check](https://github.com/markitr/tabulate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/markitr/tabulate/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/markitr/tabulate/branch/main/graph/badge.svg)](https://codecov.io/gh/markitr/tabulate?branch=main)
[![test-coverage](https://github.com/markitr/tabulate/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/markitr/tabulate/actions/workflows/test-coverage.yaml)
[![pkgdown](https://github.com/markitr/tabulate/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/markitr/tabulate/actions/workflows/pkgdown.yaml)
[![license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/markitr/tabulate/blob/main/LICENSE.md)

<!-- badges: end -->

## Overview

The goal of tabulate is to help you create tabular data in long format
from a dataframe where:

1.  The output is predictable every time.
2.  You can group and cross any variable.
3.  You can use regex to match on almost any argument.

## Installation

### Development version

To get a bug fix or to use a feature from the development version, you
can install the development version of dplyr from GitHub.

``` r
# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("markitr/tabulate")
```

------------------------------------------------------------------------

Please note that the tidyr project is released with a [Contributor Code
of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
