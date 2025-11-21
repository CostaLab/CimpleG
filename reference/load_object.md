# Load an R object saved with CimpleG or an RDS file.

Load an R object saved with CimpleG or an RDS file.

## Usage

``` r
load_object(file_name)
```

## Arguments

- file_name:

  File name in the working directory or path to file to be loaded. Files
  saved with
  [`CimpleG::save_object`](https://costalab.github.io/CimpleG/reference/save_object.md)
  and [`base::saveRDS`](https://rdrr.io/r/base/readRDS.html) files are
  supported.

## Value

the loaded R object
