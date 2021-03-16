
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CimpleG <img src="man/figures/CimpleG_v2.png" align="right" width="120" />

## Overview

CimpleG, an R package to find (simple) CpG signatures.

<!-- badges: start -->

[![R-CMD-check](https://github.com/tiagomaie/CimpleG/workflows/R-CMD-check/badge.svg)](https://github.com/tiagomaie/CimpleG/actions)
<!-- badges: end -->

## Installation

``` r
# Install directly from github (NOT YET):
devtools::install_github("tiagomaie/CimpleG")

# Alternatively, install from a local source:
#  - ie navigating through your system
install.packages(file.choose(), repos=NULL, type="source")
#  - ie given a path to a local source
install.packages("~/Downloads/CimpleG_0.0.1.XXXX.tar.gz", repos=NULL, type="source")
# or
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

# mini example with just 2 target signatures
set.seed(42)
cimpleg_result <- CimpleG(
  train_data = train_data,
  train_targets = train_targets,
  test_data = test_data,
  test_targets = test_targets,
  method="parab_scale",
  targets = c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS")
)

cimpleg_result$results
```

``` r
# check generated signatures
cimpleg_result$signatures
#> CELL_TYPE_MSCORFIBRO    CELL_TYPE_NEURONS 
#>         "cg24192660"         "cg17008486"
```

## Diff-mean/Sum-var plots

### basic plot

``` r
plt <- diffmeans_sumvariance_plot(
  data = train_data,
  target_vector = train_targets$CELL_TYPE_MSCORFIBRO==1
)
print(plt)
```

![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->

### adding color, highlighting selected features

``` r
df_dmeansvar <- compute_diffmeans_sumvar(
  data = train_data,
  target_vector = train_targets$CELL_TYPE_MSCORFIBRO==1
)

parab_param <- .7

df_dmeansvar$is_selected <- select_features(
    x = df_dmeansvar$diff_means,
    y = df_dmeansvar$sum_variance,
    a = parab_param
)

plt <- diffmeans_sumvariance_plot(
  data = df_dmeansvar,
  label_var1 = "MSC",
  color_all_points = "red",
  threshold_func = function(x,a) (a*x)^2,
  is_feature_selected_col = "is_selected",
  func_factor = parab_param
)
print(plt)
```

![](man/figures/README-unnamed-chunk-6-1.png)<!-- -->

### labeling specific features

``` r
plt <- diffmeans_sumvariance_plot(
  data = df_dmeansvar,
  feats_to_highlight = cimpleg_result$signatures
)
print(plt)
```

![](man/figures/README-unnamed-chunk-7-1.png)<!-- -->

## Deconvolution plots

### mini example with just 2 signatures

``` r
deconv_result <- deconvolution(
  CimpleG_result = cimpleg_result,
  reference_data = train_data,
  reference_targets=train_targets,
  targets=c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS"),
  new_data=test_data
)

# making color palette for our 2 classes
col_palette <- make_color_palette(c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS"))

plt <- plot_deconvolution(
  deconv_mat = deconv_result,
  name_tag = "MSCFRIBRO-NEURONS",
  sorted_classes = c("CELL_TYPE_MSCORFIBRO","CELL_TYPE_NEURONS"),
  color_palette_df=col_palette
)
print(plt$deconv_plot)
```

![](man/figures/README-unnamed-chunk-8-1.png)<!-- -->
