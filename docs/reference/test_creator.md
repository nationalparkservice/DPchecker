# Test for presence of creator and surName

`test_creator()` examines the Creator element in metadata. If the
creator element is missing, the test fails with an error. If the creator
element is present, the function looks for individual creators. If
individual creators are present and any of them lacks a surName, the
test fails with an error. If any individual creators have a surName with
more than two words, the test fails with a warning. Otherwise, the test
passes.

## Usage

``` r
test_creator(metadata = load_metadata(directory))
```

## Arguments

- metadata:

  The metadata object returned by `load_metadata`. If parameter is not
  provided, it defaults to calling `load_metadata` in the current
  project directory.

## Value

invisible(metadata)

## Examples

``` r
if (FALSE) { # \dontrun{
test_creator()
} # }
```
