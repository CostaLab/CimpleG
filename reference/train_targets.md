# Cell line train data targets

Cell line train data targets

## Usage

``` r
train_targets
```

## Format

A data frame with 18 variables for 409 samples as rows.

- `gsm`:

  GSM identifier (GEO accession number) of the sample

- `cell_type`:

  the cell type of the respective sample

- `adipocytes`:

  one-hot encoded (1 or 0) column defining if a given sample is an
  adipocyte

- `astrocytes`:

  one-hot encoded (1 or 0) column defining if a given sample is an
  astrocyte

- `blood_cells`:

  one-hot encoded (1 or 0) column defining if a given sample is a blood
  cell

- `endothelial_cells`:

  one-hot encoded (1 or 0) column defining if a given sample is an
  endothelial cell

- `epidermal_cells`:

  one-hot encoded (1 or 0) column defining if a given sample is an
  epidermal cell

- `epithelial_cells`:

  one-hot encoded (1 or 0) column defining if a given sample is an
  epithelial cell

- `fibroblasts`:

  one-hot encoded (1 or 0) column defining if a given sample is a
  fibroblast

- `glia`:

  one-hot encoded (1 or 0) column defining if a given sample is a glia
  cell

- `hepatocytes`:

  one-hot encoded (1 or 0) column defining if a given sample is an
  hepatocyte

- `ips_cells`:

  one-hot encoded (1 or 0) column defining if a given sample is an ipsc

- `msc`:

  one-hot encoded (1 or 0) column defining if a given sample is an msc

- `muscle_cells`:

  one-hot encoded (1 or 0) column defining if a given sample is a muscle
  cell

- `neurons`:

  one-hot encoded (1 or 0) column defining if a given sample is a neuron

- `muscle_sc`:

  one-hot encoded (1 or 0) column defining if a given sample is a muscle
  stem cell

- `group_data`:

  to which dataset these data belong to (`train` or `test`)

- `description`:

  the cell type of the respective sample, in long form
