# Make tidy data for use in deconvolution plots

Produces data with varied deconvolution performance metrics.

## Usage

``` r
make_deconv_pred_obs_data(
  dat,
  true_values_col,
  predicted_cols,
  sample_id_col,
  group_col
)
```

## Arguments

- dat:

  data.frame with predictions as columns, each row should be a
  prediction for a given sample and given group/celltype

- true_values_col:

  A string with the name of the column with the true values in \`dat\`.
  true values should be between 0 and 1.

- predicted_cols:

  A vector of strings with the name of the columns with the predictions
  for different methods in \`dat\`. predictions should be between 0 and
  1

- sample_id_col:

  A string with the name of the column with the sample name or ID in
  \`dat\`.

- group_col:

  A string with the name of the column containing the cell types or
  groups in \`dat\`. group col should be a factor, otherwise the
  function will make it a factor

## Value

tibble with tidied up deconvolution performance data in nested fields
