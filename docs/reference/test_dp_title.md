# Test data package title

`test_dp_title()` tests EML metadata for presence of a data package
title. The test fails with an error if the title is absent. The test
fails with a warning if the title is \> 20 or \< 5 words. Otherwise, the
test passes.

## Usage

``` r
test_dp_title(metadata = load_metadata(directory))
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
test_dp_title()
} # }
```
