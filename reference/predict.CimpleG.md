# Predict outcome from a CimpleG signatures on new data

Predict outcome from a CimpleG signatures on new data

## Usage

``` r
# S3 method for class 'CimpleG'
predict(object, ..., new_data, class_labels = NULL)
```

## Arguments

- object:

  CimpleG object.

- ...:

  Not used at the moment.

- new_data:

  Data to be predicted, samples should be in rows and features in
  columns. Last column of \`new_data\` should have the target/class
  labels coded as 0 or 1.

- class_labels:

  Class labels of new data if these are not provided directly with it.

## Value

prediction object, list with an entry for each signature
