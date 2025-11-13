# Boxplot and rankings of deconvolution metrics for deconvolution results.

Produces data with varied deconvolution performance metrics. Produces
one boxplot and one ranking plot with the for each metric.

## Usage

``` r
deconv_ranking_plot(
  deconv_df,
  true_values_col,
  predicted_cols,
  sample_id_col,
  group_col,
  metrics = c("rmse", "r_squared", "adj.r.squared", "AIC"),
  custom_colours = NULL
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

- metrics:

  A list with two entries, \`x\` and \`y\`, defining the limits of the x
  and y axis of the plot.

- custom_colours:

  A named vector with colours, where the names are the values defined in
  \`predicted_cols\`. If \`NULL\`, default colours will be used.
