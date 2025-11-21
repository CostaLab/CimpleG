# Compute diff mean sum var dataframe

Compute diff mean sum var dataframe

## Usage

``` r
compute_diffmeans_sumvar(data, target_vector)
```

## Arguments

- data:

  Matrix with beta values that will be used to compute diffmeans sumvar
  data frame

- target_vector:

  boolean vector defining which samples in data are part of the target
  class

## Value

data.frame with computed difference in means and sum of variances for
target comparison (target v others)
