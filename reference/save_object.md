# Save an R object to disk with fast and efficient compression algorithms.

Save an R object to disk with fast and efficient compression algorithms.

## Usage

``` r
save_object(object, file_name, file_format = "lz4")
```

## Arguments

- object:

  Object to be saved to disk.

- file_name:

  Name of the file where the R object is saved to.

- file_format:

  One of "lz4", "gzip", "bzip2","xz", "nocomp". `lz4` is the best
  option, fast compression and loading times, low space usage. Format
  "lz4" is only available if package `archive` is installed. Format
  "zstd" is not supported anymore as the library now needs to be
  precompiled with R.

## Value

NULL invisibly
