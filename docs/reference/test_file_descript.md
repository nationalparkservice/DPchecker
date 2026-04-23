# Test presence of file descriptions

`test_file_descript()` tests for the presence of file descriptions
(entityDescription) fields. It fails with an error if any one or all of
the entityDescription fields are empty in a dataTable. It fails with an
error if any two or more file descriptions are identical. The test fails
with a warning if for each file description that is longer than 15 words
or shorter than three words. Otherwise the test passes.

## Usage

``` r
test_file_descript(metadata = load_metadata(directory))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter is not
  provided, it defaults to calling `load_metadata` in the current
  project directory.

## Value

invisibly returns `metadata`

## Examples

``` r
if (FALSE) { # \dontrun{
test_file_descript()
} # }
```
