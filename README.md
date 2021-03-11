
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CimpleG <img src="man/figures/CimpleG.png" align="right" width="120" />

## Overview

CimpleG, an R package to find (simple) CpG signatures.

## Installation

``` r
# Install directly from github:
devtools::install_github("tiagomaie/CimpleG")

# Alternatively, install from a local source:
#  - ie navigating through your system
install.packages(file.choose(), repos=NULL, type="source")
#  - ie given a path to a local source
devtools::install_local("~/Downloads/CimpleG_0.0.1.XXXX.tar.gz")
```

## Getting started

``` r
library("CimpleG")

data(train_data)
data(train_targets)
data(test_data)
data(test_targets)

# check the train_targets table to see
# what other columns can be used as targets
# colnames(train_targets)

cimpleg_result <- CimpleG(
  train_data = train_data,
  train_targets = train_targets,
  test_data = test_data,
  test_targets = test_targets,
  targets = c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS")
)

cimpleg_result$results

cimpleg_result$signatures
```