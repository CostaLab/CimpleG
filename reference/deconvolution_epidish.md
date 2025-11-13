# EpiDISH deconvolution

EpiDISH deconvolution

## Usage

``` r
deconvolution_epidish(
  ref_mat,
  new_data,
  epidish_method = "CBS",
  epidish_nuv = seq(0.1, 1, 0.1),
  epidish_maxit = 10000
)
```

## Arguments

- ref_mat:

  Reference matrix.

- new_data:

  New data matrix.

- epidish_method:

  One of \`CBS\` (Cibersort), \`RPC\` (Robust Partial Correlations),
  \`CP\` (Constrained Projection). Default is \`CBS\`. See \`EpiDISH\`
  documentation for more information.

- epidish_nuv:

  A vector of candidate values used for svm. Only used when
  epidish_method is set to \`CBS\`. See \`EpiDISH\` documentation for
  more information.

- epidish_maxit:

  Integer with the number of max iterations for IWLS (Iterative Weighted
  Least Squares). Only used when epidish_method is set to \`RPC\`.
