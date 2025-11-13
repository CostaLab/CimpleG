# Build deconvolution reference matrix

Build deconvolution reference matrix

## Usage

``` r
make_deconv_ref_matrix(cpg_obj, ref_data, ref_data_labels, method = NULL)
```

## Arguments

- cpg_obj:

  A CimpleG object.

- ref_data:

  A matrix with the reference data to be used to build the reference
  matrix.

- ref_data_labels:

  A character vector with the true labels of the samples in the
  \`reference_data\`.

- method:

  Method used to train models in the CimpleG object. If not provided
  (NULL), method will be taken from the CimpleG object.
