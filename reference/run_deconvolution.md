# Perform deconvolution on a new set of samples, based on the CimpleG models trained

Perform deconvolution on a new set of samples, based on the CimpleG
models trained

## Usage

``` r
run_deconvolution(
  cpg_obj = NULL,
  new_data = NULL,
  ref_mat = NULL,
  deconvolution_method = c("NNLS", "EpiDISH", "NMF"),
  ...
)
```

## Arguments

- cpg_obj:

  A CimpleG object. When creating/training CimpleG the parameter
  \`deconvolution_reference\` should be set to \`TRUE\`.

- new_data:

  Matrix or data.frame that should have the samples you want to perform
  deconvolution on. Samples should be in rows and probes/CpGs in
  columns.

- ref_mat:

  If the CimpleG object does not have the reference matrix, you can
  provide it here instead. See \`make_deconv_ref_matrix\`

- deconvolution_method:

  Deconvolution method to be used. One of \#TODO

- ...:

  Extra parameters only used when deconvolution_method is set to
  \`NMF\`. The most relevant parameter are probably \`method\` and
  \`beta\`.
