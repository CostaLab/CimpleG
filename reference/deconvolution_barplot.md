# Stacked barplot of deconvolution results

Stacked barplot of deconvolution results

## Usage

``` r
deconvolution_barplot(
  deconvoluted_data,
  meta_data,
  sample_id_column,
  true_label_column,
  color_dict = NULL,
  show_x_label = FALSE,
  base_size = 14,
  ...
)
```

## Arguments

- deconvoluted_data:

  Result from running \`run_deconvolution\`

- meta_data:

  Data.frame containing metadata from deconvoluted samples

- sample_id_column:

  Name of the column containing the sample id in the meta_data
  data.frame

- true_label_column:

  Name of the column containing the true labels of the samples in the
  meta_data data.frame

- color_dict:

  Named string featuring colors as values and labels (true labels) as
  names

- show_x_label:

  A boolean, if \`TRUE\` the sample labels in the X axis will be shown.
  Default is \`FALSE\`.

- base_size:

  An integer defining the base size of the text in the plot. Default is
  \`14\`.

- ...:

  Parameters passed to the ggplot2::theme function.

## Value

A list with the data and the ggplot2 plot object.
