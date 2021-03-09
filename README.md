
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

train_d <- readRDS(file.path("data","mock_train.RDS"))
test_d <- readRDS(file.path("data","mock_test.RDS"))
train_target <- readRDS(file.path("data","mock_train_targets.RDS"))
test_target <- readRDS(file.path("data","mock_test_targets.RDS"))

cimpleg_result <- CimpleG(
  train_data = train_d,
  train_targets = train_target,
  test_data = test_d,
  test_targets = test_target,
  targets = c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS")
)

cimpleg_result$results

cimpleg_result$signatures
```
