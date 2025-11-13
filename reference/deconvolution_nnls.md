# NNLS deconvolution

NNLS deconvolution

## Usage

``` r
deconvolution_nnls(dt, compute_cols, ref_mat)
```

## Arguments

- dt:

  A data.table with the new data with features/predictions on rows and
  samples on columns.

- compute_cols:

  A character vector with the columns for which the deconvolution
  algorithm should be ran.

- ref_mat:

  The reference matrix as created by CimpleG.
