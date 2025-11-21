# Scatter plots of observed (true) vs predicted values for deconvolution.

Produces one plot panel per number of methods with predictions. Each
plot panel has one plot per cell type.

## Usage

``` r
deconv_pred_obs_plot(
  deconv_df,
  true_values_col,
  predicted_cols,
  sample_id_col,
  group_col,
  axis_lims = list(x = c(0, 1), y = c(0, 1))
)
```

## Arguments

- deconv_df:

  A data.frame with meta.data, true values and predictions for different
  methods as columns. Each row should be a prediction for a given sample
  and a given group/cell type.

- true_values_col:

  A string with the name of the column with the true values in
  \`deconv_df\`.

- predicted_cols:

  A vector of strings with the name of the columns with the predictions
  for different methods in \`deconv_df\`.

- sample_id_col:

  A string with the name of the column with the sample name or ID in
  \`deconv_df\`.

- group_col:

  A string with the name of the column containing the cell types or
  groups in \`deconv_df\`.

- axis_lims:

  A list with two entries, \`x\` and \`y\`, defining the limits of the x
  and y axis of the plot.

## Value

list of ggplot2 objects
